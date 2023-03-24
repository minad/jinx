#include <emacs-module.h>
#include <enchant.h>
#include <string.h>
#include <glib.h>

int plugin_is_GPL_compatible;

static EnchantBroker* broker = 0;

#define jinx_unused(var) _##var __attribute__((unused))

static emacs_value jinx_cons(emacs_env* env, emacs_value a, emacs_value b) {
    return env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){a, b});
}

static char* jinx_to_cstr(emacs_env* env, emacs_value str) {
    ptrdiff_t size = 0;
    if (!env->copy_string_contents(env, str, 0, &size))
        return 0;
    char* out = malloc(size);
    if (!out)
        return 0;
    if (!env->copy_string_contents(env, str, out, &size)) {
        free(out);
        return 0;
    }
    return out;
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

static emacs_value jinx_dict(emacs_env* env, ptrdiff_t jinx_unused(nargs),
                             emacs_value args[], void* jinx_unused(data)) {
    if (!broker)
        broker = enchant_broker_init();
    g_autofree char* str = jinx_to_cstr(env, args[0]);
    if (!str)
        return env->intern(env, "nil");
    EnchantDict* dict = enchant_broker_request_dict(broker, str);
    return dict
        ? env->make_user_ptr(env, jinx_free_dict, dict)
        : env->intern(env, "nil");
}

static void jinx_describe_cb(const char* const lang_tag,
                             const char* const provider_name,
                             const char* const jinx_unused(provider_desc),
                             const char* const jinx_unused(provider_file),
                             void* data) {
    emacs_env* env = ((emacs_env**)data)[0];
    ((emacs_value*)data)[1] = jinx_cons(env,
                                        env->make_string(env, lang_tag, strlen(lang_tag)),
                                        env->make_string(env, provider_name, strlen(provider_name)));
}

static emacs_value jinx_describe(emacs_env* env, ptrdiff_t jinx_unused(nargs),
                                 emacs_value args[], void* jinx_unused(data)) {
    EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
    void* data[] = { env, 0 };
    enchant_dict_describe(dict, jinx_describe_cb, data);
    return (emacs_value)data[1];
}

static emacs_value jinx_check(emacs_env* env, ptrdiff_t jinx_unused(nargs),
                              emacs_value args[], void* jinx_unused(data)) {
    g_autofree char* str = jinx_to_cstr(env, args[1]);
    if (!str)
        return env->intern(env, "nil");

    // TODO multiple dicts
    EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);

    emacs_value result = env->intern(env, "nil");

    char* ptr = str;
    ptrdiff_t idx = 0;
    while (*ptr) {
        while (*ptr && !enchant_dict_is_word_character(dict, g_utf8_get_char(ptr), 0)) {
            ++idx;
            ptr = g_utf8_next_char(ptr);
        }
        ptrdiff_t word_start_idx = idx;
        char* word_start_ptr = ptr;

        while (*ptr && enchant_dict_is_word_character(dict, g_utf8_get_char(ptr), 1)) {
            ++idx;
            ptr = g_utf8_next_char(ptr);
        }
        ptrdiff_t word_end_idx = idx;
        char* word_end_ptr = ptr;

        while (word_end_idx > word_start_idx
               && !enchant_dict_is_word_character(dict, g_utf8_get_char(g_utf8_prev_char(word_end_ptr)), 2)) {
            word_end_ptr = g_utf8_prev_char(word_end_ptr);
            --word_end_idx;
        }

        if (word_end_ptr > word_start_ptr
            && enchant_dict_check(dict, word_start_ptr, word_end_ptr - word_start_ptr))
            result = jinx_cons(env, jinx_cons(env,
                                              env->make_integer(env, word_start_idx),
                                              env->make_integer(env, word_end_idx)),
                               result);
    }

    return env->funcall(env, env->intern(env, "nreverse"), 1, (emacs_value[]){result});
}

static emacs_value jinx_add(emacs_env* env, ptrdiff_t jinx_unused(nargs),
                            emacs_value args[], void* jinx_unused(data)) {
    g_autofree char* str = jinx_to_cstr(env, args[1]);
    if (str) {
        EnchantDict* dict = (EnchantDict*)env->get_user_ptr(env, args[0]);
        enchant_dict_add(dict, str, -1);
    }
    return env->intern(env, "nil");
}

static emacs_value jinx_suggest(emacs_env* env, ptrdiff_t jinx_unused(nargs),
                                emacs_value args[], void* jinx_unused(data)) {
    g_autofree char* str = jinx_to_cstr(env, args[1]);
    if (!str)
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
    if (runtime->size < (ptrdiff_t)sizeof (*runtime))
        return 1;
    emacs_env* env = runtime->get_environment(runtime);
    jinx_defun(env, "jinx--mod-suggest", 2, 2, jinx_suggest);
    jinx_defun(env, "jinx--mod-check", 2, 2, jinx_check);
    jinx_defun(env, "jinx--mod-add", 2, 2, jinx_add);
    jinx_defun(env, "jinx--mod-dict", 1, 1, jinx_dict);
    jinx_defun(env, "jinx--mod-describe", 1, 1, jinx_describe);
    return 0;
}
