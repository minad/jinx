/* enchant
 * Copyright (C) 2003 Dom Lachowicz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * In addition, as a special exception, Dom Lachowicz
 * gives permission to link the code of this program with
 * non-LGPL Spelling Provider libraries (eg: a MSFT Office
 * spell checker backend) and distribute linked combinations including
 * the two.  You must obey the GNU Lesser General Public License in all
 * respects for all of the code used other than said providers.  If you modify
 * this file, you may extend this exception to your version of the
 * file, but you are not obligated to do so.  If you do not wish to
 * do so, delete this exception statement from your version.
 */

#ifndef ENCHANT_H
#define ENCHANT_H

#include <stdint.h> /* for uint32_t */
#include <sys/types.h> /* for size_t, ssize_t */


#ifdef __cplusplus
extern "C" {
#endif

typedef struct str_enchant_broker EnchantBroker;
typedef struct str_enchant_dict   EnchantDict;

const char *enchant_get_version (void);

/**
 * enchant_broker_init
 *
 * Returns: A new broker object capable of requesting
 * dictionaries from providers.
 */
EnchantBroker *enchant_broker_init (void);

/**
 * enchant_broker_free
 * @broker: A non-null #EnchantBroker
 *
 * Destroys the broker object. Must only be called once per broker init
 */
void enchant_broker_free (EnchantBroker * broker);

/**
 * enchant_broker_request_dict
 * @broker: A non-null #EnchantBroker
 * @tag: The non-null language tag you wish to request a dictionary for ("en_US", "de_DE", ...)
 *
 * Returns: An #EnchantDict, or %null if no suitable dictionary could be found. This dictionary is reference counted.
 */
EnchantDict *enchant_broker_request_dict (EnchantBroker * broker, const char *const tag);

/**
 * enchant_broker_request_pwl_dict
 *
 * PWL is a personal wordlist file, 1 entry per line
 *
 * @pwl: A non-null pathname in the GLib file name encoding (UTF-8 on Windows)
 *       to the personal wordlist file
 *
 * Returns: An EnchantDict. This dictionary is reference counted.
 */
EnchantDict *enchant_broker_request_pwl_dict (EnchantBroker * broker, const char *const pwl);

/**
 * enchant_broker_free_dict
 * @broker: A non-null #EnchantBroker
 * @dict: A non-null #EnchantDict
 *
 * Releases the dictionary when you are done using it. Must only be called once per dictionary request
 */
void enchant_broker_free_dict (EnchantBroker * broker, EnchantDict * dict);

/**
 * enchant_broker_dict_exists
 * @broker: A non-null #EnchantBroker
 * @tag: The non-null language tag you wish to request a dictionary for ("en_US", "de_DE", ...)
 *
 * Return existance of the requested dictionary (1 == true, 0 == false)
 */
int enchant_broker_dict_exists (EnchantBroker * broker, const char * const tag);

/**
 * enchant_broker_set_ordering
 * @broker: A non-null #EnchantBroker
 * @tag: A non-null language tag (en_US)
 * @ordering: A non-null ordering (nuspell,aspell,hunspell,hspell)
 *
 * Declares a preference of dictionaries to use for the language
 * described/referred to by @tag. The ordering is a comma delimited
 * list of provider names. As a special exception, the "*" tag can
 * be used as a language tag to declare a default ordering for any
 * language that does not explictly declare an ordering.
 */
void enchant_broker_set_ordering (EnchantBroker * broker,
                                  const char * const tag,
				  const char * const ordering);
/**
 * enchant_broker_get_error
 * @broker: A non-null broker
 *
 * Returns a const char string or NULL describing the last exception in UTF8 encoding.
 * WARNING: error is transient and is likely cleared as soon as the
 * next broker operation happens
 */
const char *enchant_broker_get_error (EnchantBroker * broker);

/**
 * EnchantBrokerDescribeFn
 * @provider_name: The provider's identifier, such as "hunspell" or "aspell", in UTF8 encoding
 * @provider_desc: A description of the provider, such as "Aspell 0.53" in UTF8 encoding
 * @provider_dll_file: The provider's DLL filename in Glib file encoding (UTF8 on Windows)
 * @user_data: Supplied user data, or %null if you don't care
 *
 * Callback used to enumerate and describe Enchant's various providers
 */
typedef void (*EnchantBrokerDescribeFn) (const char * const provider_name,
					 const char * const provider_desc,
					 const char * const provider_dll_file,
					 void * user_data);
	
/**
 * enchant_broker_describe
 * @broker: A non-null #EnchantBroker
 * @fn: A non-null #EnchantBrokerDescribeFn
 * @user_data: Optional user-data
 *
 * Enumerates the Enchant providers and tells
 * you some rudimentary information about them.
 */
void enchant_broker_describe (EnchantBroker * broker,
			      EnchantBrokerDescribeFn fn,
			      void * user_data);

/**
 * enchant_dict_check
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to check, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 *
 * Will return an "incorrect" value if any of those pre-conditions
 * are not met.
 *
 * Returns: 0 if the word is correctly spelled, positive if not, negative if error
 */
int enchant_dict_check (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_suggest
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to find suggestions for, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 * @out_n_suggs: The location to store the # of suggestions returned, or %null
 *
 * Will return an %null value if any of those pre-conditions
 * are not met.
 *
 * Returns: A %null terminated list of UTF-8 encoded suggestions, or %null
 */
char **enchant_dict_suggest (EnchantDict * dict, const char *const word,
                             ssize_t len, size_t * out_n_suggs);

/**
 * enchant_dict_add
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to add to your personal dictionary, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 *
 * Remarks: if the word exists in the exclude dictionary, it will be removed from the
 *          exclude dictionary
 */
void enchant_dict_add (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_add_to_session
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to add to this spell-checking session, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 *
 */
void enchant_dict_add_to_session (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_remove
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to add to your exclude dictionary and
 *        remove from the personal dictionary, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 *
 */
void enchant_dict_remove (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_remove_from_session
 * @dict: A non-null #EnchantDict
 * @word: The non-null word you wish to exclude from this spell-checking session, in UTF-8 encoding
 * @len: The byte length of @word, or -1 for strlen (@word)
 *
 */
void enchant_dict_remove_from_session (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_is_added
 * @dict: A non-null #EnchantDict
 * @word: The word you wish to see if it has been added (to your session or dict) in UTF8 encoding
 * @len: the byte length of @word, or -1 for strlen (@word)
 */
int enchant_dict_is_added (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_is_removed
 * @dict: A non-null #EnchantDict
 * @word: The word you wish to see if it has been removed (from your session or dict) in UTF8 encoding
 * @len: the byte length of @word, or -1 for strlen (@word)
 */
int enchant_dict_is_removed (EnchantDict * dict, const char *const word, ssize_t len);

/**
 * enchant_dict_store_replacement
 * @dict: A non-null #EnchantDict
 * @mis: The non-null word you wish to add a correction for, in UTF-8 encoding
 * @mis_len: The byte length of @mis, or -1 for strlen (@mis)
 * @cor: The non-null correction word, in UTF-8 encoding
 * @cor_len: The byte length of @cor, or -1 for strlen (@cor)
 *
 * Notes that you replaced @mis with @cor, so it's possibly more likely
 * that future occurrences of @mis will be replaced with @cor. So it might
 * bump @cor up in the suggestion list.
 */
void enchant_dict_store_replacement (EnchantDict * dict,
				     const char *const mis, ssize_t mis_len,
				     const char *const cor, ssize_t cor_len);

/**
 * enchant_dict_free_string_list
 * @dict: A non-null #EnchantDict
 * @string_list: A non-null string list returned from enchant_dict_suggest
 *
 * Releases the string list
 */
void enchant_dict_free_string_list (EnchantDict * dict, char **string_list);

/**
 * enchant_dict_get_error
 * @dict: A non-null #EnchantDict
 *
 * Returns a const char string or NULL describing the last exception in UTF8 encoding.
 * WARNING: error is transient. It will likely be cleared as soon as
 * the next dictionary operation is called.
 */
const char *enchant_dict_get_error (EnchantDict * dict);

/**
 * enchant_dict_get_extra_word_characters
 * @dict: A non-null #EnchantDict
 *
 * Returns a const char UTF-8-encoded string containing the non-letter characters
 * allowed in a word, e.g. "01234567890’-". If dash occurs, it will be last, so that
 * the string can be appended to a character class used to match word characters.
 *
 * Words containing non-letters not in this string will automatically be rejected
 * by Enchant.
 *
 * Note that for some back-ends the result may be a guess, in which case it
 * may include characters not actually allowed in the given dictionary.
 */
const char *enchant_dict_get_extra_word_characters (EnchantDict * dict);

/**
 * enchant_dict_is_word_character
 * @dict: An #EnchantDict, or %null
 * @uc: A unicode code-point
 * @n: An integer: 0 if the character is at the start of a word, 1 if it is
 * in the middle, or 2 if at the end.
 *
 * Returns a flag specifying whether the given character is valid at the
 * given position.
 *
 * One way to match a complete word is to check that the first character matches
 * with n == 0, then proceed matching characters with n == 1 until failure, then
 * proceed backwards until a character matches with n == 2.
 *
 * Note that for some back-ends the result may be a guess, in which case it
 * may allow characters not actually allowed in the given dictionary.
 *
 * If @dict is %null, a built-in implementation is used (FIXME: We should document
 * behavior for this). If @n is not 0, 1 or 2, then a false flag is returned.
 */
int enchant_dict_is_word_character (EnchantDict * dict, uint32_t uc, size_t n);

/**
 * EnchantDictDescribeFn
 * @lang_tag: The dictionary's language tag (eg: en_US, de_AT, ...)
 * @provider_name: The provider's name (eg: Aspell) in UTF8 encoding
 * @provider_desc: The provider's description (eg: Aspell 0.50.3) in UTF8 encoding
 * @provider_file: The DLL/SO where this dict's provider was loaded from in Glib file encoding (UTF8 on Windows)
 * @user_data: Supplied user data, or %null if you don't care
 *
 * Callback used to describe an individual dictionary
 */
typedef void (*EnchantDictDescribeFn) (const char * const lang_tag,
				       const char * const provider_name,
				       const char * const provider_desc,
				       const char * const provider_file,
				       void * user_data);

/**
 * enchant_dict_describe
 * @broker: A non-null #EnchantDict
 * @dict: A non-null #EnchantDictDescribeFn
 * @user_data: Optional user-data
 *
 * Describes an individual dictionary
 */
void enchant_dict_describe (EnchantDict * dict,
			    EnchantDictDescribeFn fn,
			    void * user_data);

/**
 * enchant_broker_list_dicts
 * @broker: A non-null #EnchantBroker
 * @fn: A non-null #EnchantDictDescribeFn
 * @user_data: Optional user-data
 *
 * Enumerates the dictionaries available from
 * all Enchant providers.
 */
void enchant_broker_list_dicts (EnchantBroker * broker,
				EnchantDictDescribeFn fn,
				void * user_data);

/**
 * enchant_set_prefix_dir
 *
 * Set the prefix dir. This overrides any auto-detected value,
 * and can also be used on systems or installations where
 * auto-detection does not work.
 *
 */
void enchant_set_prefix_dir(const char *);

#ifdef __cplusplus
}
#endif

#endif /* ENCHANT_H */
