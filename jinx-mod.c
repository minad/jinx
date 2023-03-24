#include <emacs-module.h>
#include <enchant.h>
#include <string.h>

int plugin_is_GPL_compatible;

static EnchantBroker* broker = 0;

static emacs_value jinx_cons(emacs_env* env, emacs_value a, emacs_value b) {
    return env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){a, b});
}

static void jinx_defun(emacs_env* env, const char* name,
                       ptrdiff_t min, ptrdiff_t max, void* fun) {
    env->funcall(env, env->intern(env, "defalias"), 2,
                 (emacs_value[]){
                     env->intern(env, name),
                     env->make_function(env, min, max, fun, 0, 0)
                 });
}

static void jinx_free_dict(void* dict) {
    enchant_broker_free_dict(broker, dict);
}

static emacs_value jinx_dict(emacs_env* env, ptrdiff_t _nargs,
                             emacs_value args[], void* _data) {
    (void)_nargs; (void)_data;
    if (!broker)
        broker = enchant_broker_init();
    char str[256];
    ptrdiff_t size = sizeof (str);
    if (!env->copy_string_contents(env, args[0], str, &size))
        return env->intern(env, "nil");
    EnchantDict* dict = enchant_broker_request_dict(broker, str);
    if (!dict)
        return env->intern(env, "nil");
    return env->make_user_ptr(env, jinx_free_dict, dict);
}

static void jinx_describe_cb(const char* const lang_tag,
                             const char* const provider_name,
                             const char* const _provider_desc,
                             const char* const _provider_file,
                             void* data) {
    (void)_provider_desc; (void)_provider_file;
    emacs_env* env = ((emacs_env**)data)[0];
    ((emacs_value*)data)[1] = jinx_cons(env,
                                        env->make_string(env, lang_tag, strlen(lang_tag)),
                                        env->make_string(env, provider_name, strlen(provider_name)));
}

static emacs_value jinx_describe(emacs_env* env, ptrdiff_t _nargs,
                                 emacs_value args[], void* _data) {
    (void)_nargs; (void)_data;
    EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
    void* data[] = { env, 0 };
    enchant_dict_describe(dict, jinx_describe_cb, data);
    return (emacs_value)data[1];
}

static emacs_value jinx_check(emacs_env* env, ptrdiff_t _nargs,
                              emacs_value args[], void* _data) {
    (void)_nargs; (void)_data;
    char str[256];
    ptrdiff_t size = sizeof (str);
    if (!env->copy_string_contents(env, args[1], str, &size))
        return env->intern(env, "nil");
    EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
    return env->intern(env, enchant_dict_check(dict, str, -1) ? "nil" : "t");
}

static emacs_value jinx_add(emacs_env* env, ptrdiff_t _nargs,
                            emacs_value args[], void* _data) {
    (void)_nargs; (void)_data;
    char str[256];
    ptrdiff_t size = sizeof (str);
    if (env->copy_string_contents(env, args[1], str, &size)) {
        EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
        enchant_dict_add(dict, str, -1);
    }
    return env->intern(env, "nil");
}

static emacs_value jinx_suggest(emacs_env* env, ptrdiff_t _nargs,
                                emacs_value args[], void* _data) {
    (void)_nargs; (void)_data;
    char str[256];
    ptrdiff_t size = sizeof (str);
    if (!env->copy_string_contents(env, args[1], str, &size))
        return env->intern(env, "nil");
    EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
    char** suggest = enchant_dict_suggest(dict, str, -1, 0);
    if (!suggest)
        return env->intern(env, "nil");
    emacs_value result = env->intern(env, "nil");
    for (; *suggest; ++suggest) {
        result = jinx_cons(env,
                           env->make_string(env, *suggest, strlen(*suggest)),
                           result);
    }
    return env->funcall(env, env->intern(env, "nreverse"), 1, (emacs_value[]){result});
}

int emacs_module_init(struct emacs_runtime *runtime) {
    if (runtime->size < sizeof (*runtime))
        return 1;
    emacs_env* env = runtime->get_environment(runtime);
    jinx_defun(env, "jinx--mod-suggest", 2, 2, jinx_suggest);
    jinx_defun(env, "jinx--mod-check", 2, 2, jinx_check);
    jinx_defun(env, "jinx--mod-add", 2, 2, jinx_add);
    jinx_defun(env, "jinx--mod-dict", 1, 1, jinx_dict);
    jinx_defun(env, "jinx--mod-describe", 1, 1, jinx_describe);
    return 0;
}
