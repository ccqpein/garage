(in-package #:tele-api-doc) 
(defparameter *all-data-types-static* '(#S(API-DATATYPE
                                           :NAME "Update"
                                           :FIELDS (#S(DATA-FIELDS-PAIRS
                                                       :FIELD "update_id"
                                                       :TYPE "Integer"
                                                       :DESCRIPTION "The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you're using <a href=\"#setwebhook\">Webhooks</a>, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "message"
                                                       :TYPE "Message"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming message of any kind — text, photo, sticker, etc.")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "edited_message"
                                                       :TYPE "Message"
                                                       :DESCRIPTION "<em>Optional</em>. New version of a message that is known to the bot and was edited")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "channel_post"
                                                       :TYPE "Message"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming channel post of any kind — text, photo, sticker, etc.")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "edited_channel_post"
                                                       :TYPE "Message"
                                                       :DESCRIPTION "<em>Optional</em>. New version of a channel post that is known to the bot and was edited")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "inline_query"
                                                       :TYPE "InlineQuery"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming <a href=\"#inline-mode\">inline</a> query")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "chosen_inline_result"
                                                       :TYPE "ChosenInlineResult"
                                                       :DESCRIPTION "<em>Optional</em>. The result of an <a href=\"#inline-mode\">inline</a> query that was chosen by a user and sent to their chat partner. Please see our documentation on the <a href=\"/bots/inline#collecting-feedback\">feedback collecting</a> for details on how to enable these updates for your bot.")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "callback_query"
                                                       :TYPE "CallbackQuery"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming callback query")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "shipping_query"
                                                       :TYPE "ShippingQuery"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming shipping query. Only for invoices with flexible price")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "pre_checkout_query"
                                                       :TYPE "PreCheckoutQuery"
                                                       :DESCRIPTION "<em>Optional</em>. New incoming pre-checkout query. Contains full information about checkout")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "poll"
                                                       :TYPE "Poll"
                                                       :DESCRIPTION "<em>Optional</em>. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot")
                                                    #S(DATA-FIELDS-PAIRS
                                                       :FIELD "poll_answer"
                                                       :TYPE "PollAnswer"
                                                       :DESCRIPTION "<em>Optional</em>. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself."))
                                           :DOC "This <a href=\"#available-types\">object</a> represents an incoming update.<br>At most <strong>one</strong> of the optional parameters can be present in any given update.") 
#S(API-DATATYPE
   :NAME "WebhookInfo"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "url"
               :TYPE "String"
               :DESCRIPTION "Webhook URL, may be empty if webhook is not set up")
            #S(DATA-FIELDS-PAIRS
               :FIELD "has_custom_certificate"
               :TYPE "Boolean"
               :DESCRIPTION "True, if a custom certificate was provided for webhook certificate checks")
            #S(DATA-FIELDS-PAIRS
               :FIELD "pending_update_count"
               :TYPE "Integer"
               :DESCRIPTION "Number of updates awaiting delivery")
            #S(DATA-FIELDS-PAIRS
               :FIELD "ip_address"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Currently used webhook IP address")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_error_date"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Unix time for the most recent error that happened when trying to deliver an update via webhook")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_error_message"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook")
            #S(DATA-FIELDS-PAIRS
               :FIELD "max_connections"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery")
            #S(DATA-FIELDS-PAIRS
               :FIELD "allowed_updates"
               :TYPE "Array of String"
               :DESCRIPTION "<em>Optional</em>. A list of update types the bot is subscribed to. Defaults to all update types"))
   :DOC "Contains information about the current status of a webhook.") 
#S(API-DATATYPE
   :NAME "User"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "Integer"
               :DESCRIPTION "Unique identifier for this user or bot")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_bot"
               :TYPE "Boolean"
               :DESCRIPTION "True, if this user is a bot")
            #S(DATA-FIELDS-PAIRS
               :FIELD "first_name"
               :TYPE "String"
               :DESCRIPTION "User's or bot's first name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User's or bot's last name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "username"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User's or bot's username")
            #S(DATA-FIELDS-PAIRS
               :FIELD "language_code"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. <a href=\"https://en.wikipedia.org/wiki/IETF_language_tag\">IETF language tag</a> of the user's language")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_join_groups"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the bot can be invited to groups. Returned only in <a href=\"#getme\">getMe</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_read_all_group_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if <a href=\"https://core.telegram.org/bots#privacy-mode\">privacy mode</a> is disabled for the bot. Returned only in <a href=\"#getme\">getMe</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "supports_inline_queries"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the bot supports inline queries. Returned only in <a href=\"#getme\">getMe</a>."))
   :DOC "This object represents a Telegram user or bot.") 
#S(API-DATATYPE
   :NAME "Chat"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "Integer"
               :DESCRIPTION "Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of chat, can be either “private”, “group”, “supergroup” or “channel”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title, for supergroups, channels and group chats")
            #S(DATA-FIELDS-PAIRS
               :FIELD "username"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Username, for private chats, supergroups and channels if available")
            #S(DATA-FIELDS-PAIRS
               :FIELD "first_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. First name of the other party in a private chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Last name of the other party in a private chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo"
               :TYPE "ChatPhoto"
               :DESCRIPTION "<em>Optional</em>. Chat photo. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "bio"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Bio of the other party in a private chat. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Description, for groups, supergroups and channel chats. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "invite_link"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Chat invite link, for groups, supergroups and channel chats. Each administrator in a chat generates their own invite links, so the bot must first generate the link using <a href=\"#exportchatinvitelink\">exportChatInviteLink</a>. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "pinned_message"
               :TYPE "Message"
               :DESCRIPTION "<em>Optional</em>. The most recent pinned message (by sending date). Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "permissions"
               :TYPE "ChatPermissions"
               :DESCRIPTION "<em>Optional</em>. Default chat member permissions, for groups and supergroups. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "slow_mode_delay"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "sticker_set_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. For supergroups, name of group sticker set. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_set_sticker_set"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the bot can change the group sticker set. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "linked_chat_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Unique identifier for the linked chat, i.e. the discussion group identifier for a channel and vice versa; for supergroups and channel chats. This identifier may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier. Returned only in <a href=\"#getchat\">getChat</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "ChatLocation"
               :DESCRIPTION "<em>Optional</em>. For supergroups, the location to which the supergroup is connected. Returned only in <a href=\"#getchat\">getChat</a>."))
   :DOC "This object represents a chat.") 
#S(API-DATATYPE
   :NAME "Message"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "message_id"
               :TYPE "Integer"
               :DESCRIPTION "Unique message identifier inside this chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. Sender, empty for messages sent to channels")
            #S(DATA-FIELDS-PAIRS
               :FIELD "sender_chat"
               :TYPE "Chat"
               :DESCRIPTION "<em>Optional</em>. Sender of the message, sent on behalf of a chat. The channel itself for channel messages. The supergroup itself for messages from anonymous group administrators. The linked channel for messages automatically forwarded to the discussion group")
            #S(DATA-FIELDS-PAIRS
               :FIELD "date"
               :TYPE "Integer"
               :DESCRIPTION "Date the message was sent in Unix time")
            #S(DATA-FIELDS-PAIRS
               :FIELD "chat"
               :TYPE "Chat"
               :DESCRIPTION "Conversation the message belongs to")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_from"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. For forwarded messages, sender of the original message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_from_chat"
               :TYPE "Chat"
               :DESCRIPTION "<em>Optional</em>. For messages forwarded from channels or from anonymous administrators, information about the original sender chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_from_message_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For messages forwarded from channels, identifier of the original message in the channel")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_signature"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. For messages forwarded from channels, signature of the post author if present")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_sender_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_date"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For forwarded messages, date the original message was sent in Unix time")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_to_message"
               :TYPE "Message"
               :DESCRIPTION "<em>Optional</em>. For replies, the original message. Note that the Message object in this field will not contain further <em>reply_to_message</em> fields even if it itself is a reply.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "via_bot"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. Bot through which the message was sent")
            #S(DATA-FIELDS-PAIRS
               :FIELD "edit_date"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Date the message was last edited in Unix time")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media_group_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. The unique identifier of a media message group this message belongs to")
            #S(DATA-FIELDS-PAIRS
               :FIELD "author_signature"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Signature of the post author for messages in channels, or the custom title of an anonymous group administrator")
            #S(DATA-FIELDS-PAIRS
               :FIELD "text"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. For text messages, the actual UTF-8 text of the message, 0-4096 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text")
            #S(DATA-FIELDS-PAIRS
               :FIELD "animation"
               :TYPE "Animation"
               :DESCRIPTION "<em>Optional</em>. Message is an animation, information about the animation. For backward compatibility, when this field is set, the <em>document</em> field will also be set")
            #S(DATA-FIELDS-PAIRS
               :FIELD "audio"
               :TYPE "Audio"
               :DESCRIPTION "<em>Optional</em>. Message is an audio file, information about the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "document"
               :TYPE "Document"
               :DESCRIPTION "<em>Optional</em>. Message is a general file, information about the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Message is a photo, available sizes of the photo")
            #S(DATA-FIELDS-PAIRS
               :FIELD "sticker"
               :TYPE "Sticker"
               :DESCRIPTION "<em>Optional</em>. Message is a sticker, information about the sticker")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video"
               :TYPE "Video"
               :DESCRIPTION "<em>Optional</em>. Message is a video, information about the video")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_note"
               :TYPE "VideoNote"
               :DESCRIPTION "<em>Optional</em>. Message is a <a href=\"https://telegram.org/blog/video-messages-and-telescope\">video note</a>, information about the video message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "voice"
               :TYPE "Voice"
               :DESCRIPTION "<em>Optional</em>. Message is a voice message, information about the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption for the animation, audio, document, photo, video or voice, 0-1024 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption")
            #S(DATA-FIELDS-PAIRS
               :FIELD "contact"
               :TYPE "Contact"
               :DESCRIPTION "<em>Optional</em>. Message is a shared contact, information about the contact")
            #S(DATA-FIELDS-PAIRS
               :FIELD "dice"
               :TYPE "Dice"
               :DESCRIPTION "<em>Optional</em>. Message is a dice with random value")
            #S(DATA-FIELDS-PAIRS
               :FIELD "game"
               :TYPE "Game"
               :DESCRIPTION "<em>Optional</em>. Message is a game, information about the game. <a href=\"#games\">More about games »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "poll"
               :TYPE "Poll"
               :DESCRIPTION "<em>Optional</em>. Message is a native poll, information about the poll")
            #S(DATA-FIELDS-PAIRS
               :FIELD "venue"
               :TYPE "Venue"
               :DESCRIPTION "<em>Optional</em>. Message is a venue, information about the venue. For backward compatibility, when this field is set, the <em>location</em> field will also be set")
            #S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "Location"
               :DESCRIPTION "<em>Optional</em>. Message is a shared location, information about the location")
            #S(DATA-FIELDS-PAIRS
               :FIELD "new_chat_members"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "left_chat_member"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. A member was removed from the group, information about them (this member may be the bot itself)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "new_chat_title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. A chat title was changed to this value")
            #S(DATA-FIELDS-PAIRS
               :FIELD "new_chat_photo"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. A chat photo was change to this value")
            #S(DATA-FIELDS-PAIRS
               :FIELD "delete_chat_photo"
               :TYPE "True"
               :DESCRIPTION "<em>Optional</em>. Service message: the chat photo was deleted")
            #S(DATA-FIELDS-PAIRS
               :FIELD "group_chat_created"
               :TYPE "True"
               :DESCRIPTION "<em>Optional</em>. Service message: the group has been created")
            #S(DATA-FIELDS-PAIRS
               :FIELD "supergroup_chat_created"
               :TYPE "True"
               :DESCRIPTION "<em>Optional</em>. Service message: the supergroup has been created. This field can't be received in a message coming through updates, because bot can't be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "channel_chat_created"
               :TYPE "True"
               :DESCRIPTION "<em>Optional</em>. Service message: the channel has been created. This field can't be received in a message coming through updates, because bot can't be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "migrate_to_chat_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "migrate_from_chat_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "pinned_message"
               :TYPE "Message"
               :DESCRIPTION "<em>Optional</em>. Specified message was pinned. Note that the Message object in this field will not contain further <em>reply_to_message</em> fields even if it is itself a reply.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "invoice"
               :TYPE "Invoice"
               :DESCRIPTION "<em>Optional</em>. Message is an invoice for a <a href=\"#payments\">payment</a>, information about the invoice. <a href=\"#payments\">More about payments »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "successful_payment"
               :TYPE "SuccessfulPayment"
               :DESCRIPTION "<em>Optional</em>. Message is a service message about a successful payment, information about the payment. <a href=\"#payments\">More about payments »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "connected_website"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. The domain name of the website on which the user has logged in. <a href=\"/widgets/login\">More about Telegram Login »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "passport_data"
               :TYPE "PassportData"
               :DESCRIPTION "<em>Optional</em>. Telegram Passport data")
            #S(DATA-FIELDS-PAIRS
               :FIELD "proximity_alert_triggered"
               :TYPE "ProximityAlertTriggered"
               :DESCRIPTION "<em>Optional</em>. Service message. A user in the chat triggered another user's proximity alert while sharing Live Location.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. Inline keyboard attached to the message. <code>login_url</code> buttons are represented as ordinary <code>url</code> buttons."))
   :DOC "This object represents a message.") 
#S(API-DATATYPE
   :NAME "MessageId"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "message_id"
               :TYPE "Integer"
               :DESCRIPTION "Unique message identifier"))
   :DOC "This object represents a unique message identifier.") 
#S(API-DATATYPE
   :NAME "MessageEntity"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the entity. Can be “mention” (<code>@username</code>), “hashtag” (<code>#hashtag</code>), “cashtag” (<code>$USD</code>), “bot_command” (<code>/start@jobs_bot</code>), “url” (<code>https://telegram.org</code>), “email” (<code>do-not-reply@telegram.org</code>), “phone_number” (<code>+1-212-555-0123</code>), “bold” (<strong>bold text</strong>), “italic” (<em>italic text</em>), “underline” (underlined text), “strikethrough” (strikethrough text), “code” (monowidth string), “pre” (monowidth block), “text_link” (for clickable text URLs), “text_mention” (for users <a href=\"https://telegram.org/blog/edit#new-mentions\">without usernames</a>)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "offset"
               :TYPE "Integer"
               :DESCRIPTION "Offset in UTF-16 code units to the start of the entity")
            #S(DATA-FIELDS-PAIRS
               :FIELD "length"
               :TYPE "Integer"
               :DESCRIPTION "Length of the entity in UTF-16 code units")
            #S(DATA-FIELDS-PAIRS
               :FIELD "url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. For “text_link” only, url that will be opened after user taps on the text")
            #S(DATA-FIELDS-PAIRS
               :FIELD "user"
               :TYPE "User"
               :DESCRIPTION "<em>Optional</em>. For “text_mention” only, the mentioned user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "language"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. For “pre” only, the programming language of the entity text"))
   :DOC "This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.") 
#S(API-DATATYPE
   :NAME "PhotoSize"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "Photo width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "Photo height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents one size of a photo or a <a href=\"#document\">file</a> / <a href=\"#sticker\">sticker</a> thumbnail.") 
#S(API-DATATYPE
   :NAME "Animation"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "Video width as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "Video height as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "Duration of the video in seconds as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Animation thumbnail as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Original animation filename as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the file as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents an animation file (GIF or H.264/MPEG-4 AVC video without sound).") 
#S(API-DATATYPE
   :NAME "Audio"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "Duration of the audio in seconds as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "performer"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Performer of the audio as defined by sender or by audio tags")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title of the audio as defined by sender or by audio tags")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Original filename as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the file as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Thumbnail of the album cover to which the music file belongs"))
   :DOC "This object represents an audio file to be treated as music by the Telegram clients.") 
#S(API-DATATYPE
   :NAME "Document"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Document thumbnail as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Original filename as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the file as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents a general file (as opposed to <a href=\"#photosize\">photos</a>, <a href=\"#voice\">voice messages</a> and <a href=\"#audio\">audio files</a>).") 
#S(API-DATATYPE
   :NAME "Video"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "Video width as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "Video height as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "Duration of the video in seconds as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Video thumbnail")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Original filename as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mime type of a file as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents a video file.") 
#S(API-DATATYPE
   :NAME "VideoNote"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "length"
               :TYPE "Integer"
               :DESCRIPTION "Video width and height (diameter of the video message) as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "Duration of the video in seconds as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Video thumbnail")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents a <a href=\"https://telegram.org/blog/video-messages-and-telescope\">video message</a> (available in Telegram apps as of <a href=\"https://telegram.org/blog/video-messages-and-telescope\">v.4.0</a>).") 
#S(API-DATATYPE
   :NAME "Voice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "Duration of the audio in seconds as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the file as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents a voice note.") 
#S(API-DATATYPE
   :NAME "Contact"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "phone_number"
               :TYPE "String"
               :DESCRIPTION "Contact's phone number")
            #S(DATA-FIELDS-PAIRS
               :FIELD "first_name"
               :TYPE "String"
               :DESCRIPTION "Contact's first name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Contact's last name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "user_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Contact's user identifier in Telegram")
            #S(DATA-FIELDS-PAIRS
               :FIELD "vcard"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Additional data about the contact in the form of a <a href=\"https://en.wikipedia.org/wiki/VCard\">vCard</a>"))
   :DOC "This object represents a phone contact.") 
#S(API-DATATYPE
   :NAME "Dice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "emoji"
               :TYPE "String"
               :DESCRIPTION "Emoji on which the dice throw animation is based")
            #S(DATA-FIELDS-PAIRS
               :FIELD "value"
               :TYPE "Integer"
               :DESCRIPTION "Value of the dice, 1-6 for “<img src=\"//telegram.org/img/emoji/40/F09F8EB2.png\" alt=\"🎲\" class=\"emoji\" height=\"20\" width=\"20\">” and “<img src=\"//telegram.org/img/emoji/40/F09F8EAF.png\" height=\"20\" width=\"20\" alt=\"🎯\" class=\"emoji\">” base emoji, 1-5 for “<img alt=\"🏀\" src=\"//telegram.org/img/emoji/40/F09F8F80.png\" width=\"20\" height=\"20\" class=\"emoji\">” and “<img height=\"20\" alt=\"⚽\" class=\"emoji\" src=\"//telegram.org/img/emoji/40/E29ABD.png\" width=\"20\">” base emoji, 1-64 for “<img height=\"20\" class=\"emoji\" width=\"20\" alt=\"🎰\" src=\"//telegram.org/img/emoji/40/F09F8EB0.png\">” base emoji"))
   :DOC "This object represents an animated emoji that displays a random value.") 
#S(API-DATATYPE
   :NAME "PollOption"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "text"
               :TYPE "String"
               :DESCRIPTION "Option text, 1-100 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "voter_count"
               :TYPE "Integer"
               :DESCRIPTION "Number of users that voted for this option"))
   :DOC "This object contains information about one answer option in a poll.") 
#S(API-DATATYPE
   :NAME "PollAnswer"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "poll_id"
               :TYPE "String"
               :DESCRIPTION "Unique poll identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "user"
               :TYPE "User"
               :DESCRIPTION "The user, who changed the answer to the poll")
            #S(DATA-FIELDS-PAIRS
               :FIELD "option_ids"
               :TYPE "Array of Integer"
               :DESCRIPTION "0-based identifiers of answer options, chosen by the user. May be empty if the user retracted their vote."))
   :DOC "This object represents an answer of a user in a non-anonymous poll.") 
#S(API-DATATYPE
   :NAME "Poll"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique poll identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "question"
               :TYPE "String"
               :DESCRIPTION "Poll question, 1-255 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "options"
               :TYPE "PollOption"
               :DESCRIPTION "List of poll options")
            #S(DATA-FIELDS-PAIRS
               :FIELD "total_voter_count"
               :TYPE "Integer"
               :DESCRIPTION "Total number of users that voted in the poll")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_closed"
               :TYPE "Boolean"
               :DESCRIPTION "True, if the poll is closed")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_anonymous"
               :TYPE "Boolean"
               :DESCRIPTION "True, if the poll is anonymous")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Poll type, currently can be “regular” or “quiz”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "allows_multiple_answers"
               :TYPE "Boolean"
               :DESCRIPTION "True, if the poll allows multiple answers")
            #S(DATA-FIELDS-PAIRS
               :FIELD "correct_option_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. 0-based identifier of the correct answer option. Available only for polls in the quiz mode, which are closed, or was sent (not forwarded) by the bot or to the private chat with the bot.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "explanation"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "explanation_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. Special entities like usernames, URLs, bot commands, etc. that appear in the <em>explanation</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "open_period"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Amount of time in seconds the poll will be active after creation")
            #S(DATA-FIELDS-PAIRS
               :FIELD "close_date"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Point in time (Unix timestamp) when the poll will be automatically closed"))
   :DOC "This object contains information about a poll.") 
#S(API-DATATYPE
   :NAME "Location"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "longitude"
               :TYPE "Float"
               :DESCRIPTION "Longitude as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "latitude"
               :TYPE "Float"
               :DESCRIPTION "Latitude as defined by sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "horizontal_accuracy"
               :TYPE "Float number"
               :DESCRIPTION "<em>Optional</em>. The radius of uncertainty for the location, measured in meters; 0-1500")
            #S(DATA-FIELDS-PAIRS
               :FIELD "live_period"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Time relative to the message sending date, during which the location can be updated, in seconds. For active live locations only.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "heading"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. The direction in which user is moving, in degrees; 1-360. For active live locations only.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "proximity_alert_radius"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Maximum distance for proximity alerts about approaching another chat member, in meters. For sent live locations only."))
   :DOC "This object represents a point on the map.") 
#S(API-DATATYPE
   :NAME "Venue"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "Location"
               :DESCRIPTION "Venue location. Can't be a live location")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Name of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "address"
               :TYPE "String"
               :DESCRIPTION "Address of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare identifier of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare type of the venue. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places identifier of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places type of the venue. (See <a href=\"https://developers.google.com/places/web-service/supported_types\">supported types</a>.)"))
   :DOC "This object represents a venue.") 
#S(API-DATATYPE
   :NAME "ProximityAlertTriggered"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "traveler"
               :TYPE "User"
               :DESCRIPTION "User that triggered the alert")
            #S(DATA-FIELDS-PAIRS
               :FIELD "watcher"
               :TYPE "User"
               :DESCRIPTION "User that set the alert")
            #S(DATA-FIELDS-PAIRS
               :FIELD "distance"
               :TYPE "Integer"
               :DESCRIPTION "The distance between the users"))
   :DOC "This object represents the content of a service message, sent whenever a user in the chat triggers a proximity alert set by another user.") 
#S(API-DATATYPE
   :NAME "UserProfilePhotos"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "total_count"
               :TYPE "Integer"
               :DESCRIPTION "Total number of profile pictures the target user has")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photos"
               :TYPE "PhotoSize"
               :DESCRIPTION "Requested profile pictures (in up to 4 sizes each)"))
   :DOC "This object represent a user's profile pictures.") 
#S(API-DATATYPE
   :NAME "File"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size, if known")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_path"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. File path. Use <code>https://api.telegram.org/file/bot&lt;token&gt;/&lt;file_path&gt;</code> to get the file."))
   :DOC "This object represents a file ready to be downloaded. The file can be downloaded via the link <code>https://api.telegram.org/file/bot&lt;token&gt;/&lt;file_path&gt;</code>. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling <a href=\"#getfile\">getFile</a>.") 
#S(API-DATATYPE
   :NAME "ReplyKeyboardMarkup"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "keyboard"
               :TYPE "KeyboardButton"
               :DESCRIPTION "Array of button rows, each represented by an Array of <a href=\"#keyboardbutton\">KeyboardButton</a> objects")
            #S(DATA-FIELDS-PAIRS
               :FIELD "resize_keyboard"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to <em>false</em>, in which case the custom keyboard is always of the same height as the app's standard keyboard.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "one_time_keyboard"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to <em>false</em>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "selective"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the <em>text</em> of the <a href=\"#message\">Message</a> object; 2) if the bot's message is a reply (has <em>reply_to_message_id</em>), sender of the original message.<br><br><em>Example:</em> A user requests to change the bot's language, bot replies to the request with a keyboard to select the new language. Other users in the group don't see the keyboard."))
   :DOC "This object represents a <a href=\"https://core.telegram.org/bots#keyboards\">custom keyboard</a> with reply options (see <a href=\"https://core.telegram.org/bots#keyboards\">Introduction to bots</a> for details and examples).") 
#S(API-DATATYPE
   :NAME "KeyboardButton"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "text"
               :TYPE "String"
               :DESCRIPTION "Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed")
            #S(DATA-FIELDS-PAIRS
               :FIELD "request_contact"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. If <em>True</em>, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only")
            #S(DATA-FIELDS-PAIRS
               :FIELD "request_location"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. If <em>True</em>, the user's current location will be sent when the button is pressed. Available in private chats only")
            #S(DATA-FIELDS-PAIRS
               :FIELD "request_poll"
               :TYPE "KeyboardButtonPollType"
               :DESCRIPTION "<em>Optional</em>. If specified, the user will be asked to create a poll and send it to the bot when the button is pressed. Available in private chats only"))
   :DOC "This object represents one button of the reply keyboard. For simple text buttons <em>String</em> can be used instead of this object to specify text of the button. Optional fields <em>request_contact</em>, <em>request_location</em>, and <em>request_poll</em> are mutually exclusive.") 
#S(API-DATATYPE
   :NAME "KeyboardButtonPollType"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. If <em>quiz</em> is passed, the user will be allowed to create only polls in the quiz mode. If <em>regular</em> is passed, only regular polls will be allowed. Otherwise, the user will be allowed to create a poll of any type."))
   :DOC "This object represents type of a poll, which is allowed to be created and sent when the corresponding button is pressed.") 
#S(API-DATATYPE
   :NAME "ReplyKeyboardRemove"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "remove_keyboard"
               :TYPE "True"
               :DESCRIPTION "Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use <em>one_time_keyboard</em> in <a href=\"#replykeyboardmarkup\">ReplyKeyboardMarkup</a>)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "selective"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Use this parameter if you want to remove the keyboard for specific users only. Targets: 1) users that are @mentioned in the <em>text</em> of the <a href=\"#message\">Message</a> object; 2) if the bot's message is a reply (has <em>reply_to_message_id</em>), sender of the original message.<br><br><em>Example:</em> A user votes in a poll, bot returns confirmation message in reply to the vote and removes the keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet."))
   :DOC "Upon receiving a message with this object, Telegram clients will remove the current custom keyboard and display the default letter-keyboard. By default, custom keyboards are displayed until a new keyboard is sent by a bot. An exception is made for one-time keyboards that are hidden immediately after the user presses a button (see <a href=\"#replykeyboardmarkup\">ReplyKeyboardMarkup</a>).") 
#S(API-DATATYPE
   :NAME "InlineKeyboardMarkup"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "inline_keyboard"
               :TYPE "InlineKeyboardButton"
               :DESCRIPTION "Array of button rows, each represented by an Array of <a href=\"#inlinekeyboardbutton\">InlineKeyboardButton</a> objects"))
   :DOC "This object represents an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a> that appears right next to the message it belongs to.") 
#S(API-DATATYPE
   :NAME "InlineKeyboardButton"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "text"
               :TYPE "String"
               :DESCRIPTION "Label text on the button")
            #S(DATA-FIELDS-PAIRS
               :FIELD "url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. HTTP or tg:// url to be opened when button is pressed")
            #S(DATA-FIELDS-PAIRS
               :FIELD "login_url"
               :TYPE "LoginUrl"
               :DESCRIPTION "<em>Optional</em>. An HTTP URL used to automatically authorize the user. Can be used as a replacement for the <a href=\"https://core.telegram.org/widgets/login\">Telegram Login Widget</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "callback_data"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Data to be sent in a <a href=\"#callbackquery\">callback query</a> to the bot when button is pressed, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "switch_inline_query"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot's username and the specified inline query in the input field. Can be empty, in which case just the bot's username will be inserted.<br><br><strong>Note:</strong> This offers an easy way for users to start using your bot in <a href=\"/bots/inline\">inline mode</a> when they are currently in a private chat with it. Especially useful when combined with <a href=\"#answerinlinequery\"><em>switch_pm…</em></a> actions – in this case the user will be automatically returned to the chat they switched from, skipping the chat selection screen.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "switch_inline_query_current_chat"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. If set, pressing the button will insert the bot's username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot's username will be inserted.<br><br>This offers a quick way for the user to open your bot in inline mode in the same chat – good for selecting something from multiple options.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "callback_game"
               :TYPE "CallbackGame"
               :DESCRIPTION "<em>Optional</em>. Description of the game that will be launched when the user presses the button.<br><br><strong>NOTE:</strong> This type of button <strong>must</strong> always be the first button in the first row.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "pay"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Specify True, to send a <a href=\"#payments\">Pay button</a>.<br><br><strong>NOTE:</strong> This type of button <strong>must</strong> always be the first button in the first row."))
   :DOC "This object represents one button of an inline keyboard. You <strong>must</strong> use exactly one of the optional fields.") 
#S(API-DATATYPE
   :NAME "LoginUrl"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "url"
               :TYPE "String"
               :DESCRIPTION "An HTTP URL to be opened with user authorization data added to the query string when the button is pressed. If the user refuses to provide authorization data, the original URL without information about the user will be opened. The data added is the same as described in <a href=\"https://core.telegram.org/widgets/login#receiving-authorization-data\">Receiving authorization data</a>.<br><br><strong>NOTE:</strong> You <strong>must</strong> always check the hash of the received data to verify the authentication and the integrity of the data as described in <a href=\"https://core.telegram.org/widgets/login#checking-authorization\">Checking authorization</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "forward_text"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. New text of the button in forwarded messages.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "bot_username"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Username of a bot, which will be used for user authorization. See <a href=\"https://core.telegram.org/widgets/login#setting-up-a-bot\">Setting up a bot</a> for more details. If not specified, the current bot's username will be assumed. The <em>url</em>'s domain must be the same as the domain linked with the bot. See <a href=\"https://core.telegram.org/widgets/login#linking-your-domain-to-the-bot\">Linking your domain to the bot</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "request_write_access"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Pass True to request the permission for your bot to send messages to the user."))
   :DOC "Telegram apps support these buttons as of <a href=\"https://telegram.org/blog/privacy-discussions-web-bots#meet-seamless-web-bots\">version 5.7</a>.") 
#S(API-DATATYPE
   :NAME "CallbackQuery"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this query")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "Sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "Message"
               :DESCRIPTION "<em>Optional</em>. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old")
            #S(DATA-FIELDS-PAIRS
               :FIELD "inline_message_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Identifier of the message sent via the bot in inline mode, that originated the query.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "chat_instance"
               :TYPE "String"
               :DESCRIPTION "Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in <a href=\"#games\">games</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "data"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "game_short_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short name of a <a href=\"#games\">Game</a> to be returned, serves as the unique identifier for the game"))
   :DOC "This object represents an incoming callback query from a callback button in an <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>. If the button that originated the query was attached to a message sent by the bot, the field <em>message</em> will be present. If the button was attached to a message sent via the bot (in <a href=\"#inline-mode\">inline mode</a>), the field <em>inline_message_id</em> will be present. Exactly one of the fields <em>data</em> or <em>game_short_name</em> will be present.") 
#S(API-DATATYPE
   :NAME "ForceReply"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "force_reply"
               :TYPE "True"
               :DESCRIPTION "Shows reply interface to the user, as if they manually selected the bot's message and tapped 'Reply'")
            #S(DATA-FIELDS-PAIRS
               :FIELD "selective"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the <em>text</em> of the <a href=\"#message\">Message</a> object; 2) if the bot's message is a reply (has <em>reply_to_message_id</em>), sender of the original message."))
   :DOC "Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot's message and tapped 'Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice <a href=\"/bots#privacy-mode\">privacy mode</a>.") 
#S(API-DATATYPE
   :NAME "ChatPhoto"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "small_file_id"
               :TYPE "String"
               :DESCRIPTION "File identifier of small (160x160) chat photo. This file_id can be used only for photo download and only for as long as the photo is not changed.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "small_file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique file identifier of small (160x160) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "big_file_id"
               :TYPE "String"
               :DESCRIPTION "File identifier of big (640x640) chat photo. This file_id can be used only for photo download and only for as long as the photo is not changed.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "big_file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique file identifier of big (640x640) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file."))
   :DOC "This object represents a chat photo.") 
#S(API-DATATYPE
   :NAME "ChatMember"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "user"
               :TYPE "User"
               :DESCRIPTION "Information about the user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "status"
               :TYPE "String"
               :DESCRIPTION "The member's status in the chat. Can be “creator”, “administrator”, “member”, “restricted”, “left” or “kicked”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "custom_title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Owner and administrators only. Custom title for this user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_anonymous"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Owner and administrators only. True, if the user's presence in the chat is hidden")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_be_edited"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the bot is allowed to edit administrator privileges of that user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_post_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the administrator can post in the channel; channels only")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_edit_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the administrator can edit messages of other users and can pin messages; channels only")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_delete_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the administrator can delete messages of other users")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_restrict_members"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the administrator can restrict, ban or unban chat members")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_promote_members"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators only. True, if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_change_info"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators and restricted only. True, if the user is allowed to change the chat title, photo and other settings")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_invite_users"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators and restricted only. True, if the user is allowed to invite new users to the chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_pin_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Administrators and restricted only. True, if the user is allowed to pin messages; groups and supergroups only")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_member"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is a member of the chat at the moment of the request")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is allowed to send text messages, contacts, locations and venues")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_media_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is allowed to send audios, documents, photos, videos, video notes and voice notes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_polls"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is allowed to send polls")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_other_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is allowed to send animations, games, stickers and use inline bots")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_add_web_page_previews"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Restricted only. True, if the user is allowed to add web page previews to their messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "until_date"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Restricted and kicked only. Date when restrictions will be lifted for this user; unix time"))
   :DOC "This object contains information about one member of a chat.") 
#S(API-DATATYPE
   :NAME "ChatPermissions"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "can_send_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to send text messages, contacts, locations and venues")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_media_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_polls"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to send polls, implies can_send_messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_send_other_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to send animations, games, stickers and use inline bots, implies can_send_media_messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_add_web_page_previews"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to add web page previews to their messages, implies can_send_media_messages")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_change_info"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_invite_users"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to invite new users to the chat")
            #S(DATA-FIELDS-PAIRS
               :FIELD "can_pin_messages"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. True, if the user is allowed to pin messages. Ignored in public supergroups"))
   :DOC "Describes actions that a non-administrator user is allowed to take in a chat.") 
#S(API-DATATYPE
   :NAME "ChatLocation"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "Location"
               :DESCRIPTION "The location to which the supergroup is connected. Can't be a live location.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "address"
               :TYPE "String"
               :DESCRIPTION "Location address; 1-64 characters, as defined by the chat owner"))
   :DOC "Represents a location to which a chat is connected.") 
#S(API-DATATYPE
   :NAME "BotCommand"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "command"
               :TYPE "String"
               :DESCRIPTION "Text of the command, 1-32 characters. Can contain only lowercase English letters, digits and underscores.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "Description of the command, 3-256 characters."))
   :DOC "This object represents a bot command.") 
#S(API-DATATYPE
   :NAME "ResponseParameters"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "migrate_to_chat_id"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "retry_after"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. In case of exceeding flood control, the number of seconds left to wait before the request can be repeated"))
   :DOC "Contains information about why a request was unsuccessful.") 
#S(API-DATATYPE
   :NAME "InputMediaPhoto"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>photo</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media"
               :TYPE "String"
               :DESCRIPTION "File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://&lt;file_attach_name&gt;” to upload a new one using multipart/form-data under &lt;file_attach_name&gt; name. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the photo to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the photo caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>"))
   :DOC "Represents a photo to be sent.") 
#S(API-DATATYPE
   :NAME "InputMediaVideo"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>video</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media"
               :TYPE "String"
               :DESCRIPTION "File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://&lt;file_attach_name&gt;” to upload a new one using multipart/form-data under &lt;file_attach_name&gt; name. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "InputFile"
               :DESCRIPTION "<em>Optional</em>. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the video to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the video caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video duration")
            #S(DATA-FIELDS-PAIRS
               :FIELD "supports_streaming"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Pass <em>True</em>, if the uploaded video is suitable for streaming"))
   :DOC "Represents a video to be sent.") 
#S(API-DATATYPE
   :NAME "InputMediaAnimation"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>animation</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media"
               :TYPE "String"
               :DESCRIPTION "File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://&lt;file_attach_name&gt;” to upload a new one using multipart/form-data under &lt;file_attach_name&gt; name. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "InputFile"
               :DESCRIPTION "<em>Optional</em>. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the animation to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the animation caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Animation width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Animation height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Animation duration"))
   :DOC "Represents an animation file (GIF or H.264/MPEG-4 AVC video without sound) to be sent.") 
#S(API-DATATYPE
   :NAME "InputMediaAudio"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>audio</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media"
               :TYPE "String"
               :DESCRIPTION "File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://&lt;file_attach_name&gt;” to upload a new one using multipart/form-data under &lt;file_attach_name&gt; name. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "InputFile"
               :DESCRIPTION "<em>Optional</em>. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the audio to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the audio caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Duration of the audio in seconds")
            #S(DATA-FIELDS-PAIRS
               :FIELD "performer"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Performer of the audio")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title of the audio"))
   :DOC "Represents an audio file to be treated as music to be sent.") 
#S(API-DATATYPE
   :NAME "InputMediaDocument"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>document</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "media"
               :TYPE "String"
               :DESCRIPTION "File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://&lt;file_attach_name&gt;” to upload a new one using multipart/form-data under &lt;file_attach_name&gt; name. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "InputFile"
               :DESCRIPTION "<em>Optional</em>. Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the document to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the document caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "disable_content_type_detection"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Disables automatic server-side content type detection for files uploaded using multipart/form-data. Always true, if the document is sent as part of an album."))
   :DOC "Represents a general file to be sent.") 
#S(API-DATATYPE
   :NAME "Sticker"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "width"
               :TYPE "Integer"
               :DESCRIPTION "Sticker width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "height"
               :TYPE "Integer"
               :DESCRIPTION "Sticker height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_animated"
               :TYPE "Boolean"
               :DESCRIPTION "<em>True</em>, if the sticker is <a href=\"https://telegram.org/blog/animated-stickers\">animated</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Sticker thumbnail in the .WEBP or .JPG format")
            #S(DATA-FIELDS-PAIRS
               :FIELD "emoji"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Emoji associated with the sticker")
            #S(DATA-FIELDS-PAIRS
               :FIELD "set_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Name of the sticker set to which the sticker belongs")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mask_position"
               :TYPE "MaskPosition"
               :DESCRIPTION "<em>Optional</em>. For mask stickers, the position where the mask should be placed")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. File size"))
   :DOC "This object represents a sticker.") 
#S(API-DATATYPE
   :NAME "StickerSet"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "name"
               :TYPE "String"
               :DESCRIPTION "Sticker set name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Sticker set title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "is_animated"
               :TYPE "Boolean"
               :DESCRIPTION "<em>True</em>, if the sticker set contains <a href=\"https://telegram.org/blog/animated-stickers\">animated stickers</a>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "contains_masks"
               :TYPE "Boolean"
               :DESCRIPTION "<em>True</em>, if the sticker set contains masks")
            #S(DATA-FIELDS-PAIRS
               :FIELD "stickers"
               :TYPE "Sticker"
               :DESCRIPTION "List of all set stickers")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb"
               :TYPE "PhotoSize"
               :DESCRIPTION "<em>Optional</em>. Sticker set thumbnail in the .WEBP or .TGS format"))
   :DOC "This object represents a sticker set.") 
#S(API-DATATYPE
   :NAME "MaskPosition"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "point"
               :TYPE "String"
               :DESCRIPTION "The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "x_shift"
               :TYPE "Float number"
               :DESCRIPTION "Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "y_shift"
               :TYPE "Float number"
               :DESCRIPTION "Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "scale"
               :TYPE "Float number"
               :DESCRIPTION "Mask scaling coefficient. For example, 2.0 means double size."))
   :DOC "This object describes the position on faces where a mask should be placed by default.") 
#S(API-DATATYPE
   :NAME "InlineQuery"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this query")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "Sender")
            #S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "Location"
               :DESCRIPTION "<em>Optional</em>. Sender location, only for bots that request user location")
            #S(DATA-FIELDS-PAIRS
               :FIELD "query"
               :TYPE "String"
               :DESCRIPTION "Text of the query (up to 256 characters)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "offset"
               :TYPE "String"
               :DESCRIPTION "Offset of the results to be returned, can be controlled by the bot"))
   :DOC "This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultArticle"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>article</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 Bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "Content of the message to be sent")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. URL of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "hide_url"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Pass <em>True</em>, if you don't want the URL to be shown in the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Url of the thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail height"))
   :DOC "Represents a link to an article or web page.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultPhoto"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>photo</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL of the photo. Photo must be in <strong>jpeg</strong> format. Photo size must not exceed 5MB")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "URL of the thumbnail for the photo")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Width of the photo")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Height of the photo")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the photo to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the photo caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the photo"))
   :DOC "Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the photo.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultGif"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>gif</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "gif_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the GIF file. File size must not exceed 1MB")
            #S(DATA-FIELDS-PAIRS
               :FIELD "gif_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Width of the GIF")
            #S(DATA-FIELDS-PAIRS
               :FIELD "gif_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Height of the GIF")
            #S(DATA-FIELDS-PAIRS
               :FIELD "gif_duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Duration of the GIF")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "URL of the static (JPEG or GIF) or animated (MPEG4) thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the thumbnail, must be one of “image/jpeg”, “image/gif”, or “video/mp4”. Defaults to “image/jpeg”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the GIF file to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the GIF animation"))
   :DOC "Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the animation.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultMpeg4Gif"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>mpeg4_gif</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mpeg4_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the MP4 file. File size must not exceed 1MB")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mpeg4_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mpeg4_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mpeg4_duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video duration")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "URL of the static (JPEG or GIF) or animated (MPEG4) thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_mime_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. MIME type of the thumbnail, must be one of “image/jpeg”, “image/gif”, or “video/mp4”. Defaults to “image/jpeg”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the MPEG-4 file to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the video animation"))
   :DOC "Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the animation.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultVideo"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>video</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the embedded video player or video file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "Mime type of the content of video url, “text/html” or “video/mp4”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "URL of the thumbnail (jpeg only) for the video")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the video to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the video caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video height")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Video duration in seconds")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the video. This field is <strong>required</strong> if InlineQueryResultVideo is used to send an HTML-page as a result (e.g., a YouTube video)."))
   :DOC "Represents a link to a page containing an embedded video player or a video file. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the video.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultAudio"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>audio</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "audio_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the audio file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the audio caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "performer"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Performer")
            #S(DATA-FIELDS-PAIRS
               :FIELD "audio_duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Audio duration in seconds")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the audio"))
   :DOC "Represents a link to an MP3 audio file. By default, this audio file will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the audio.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultVoice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>voice</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "voice_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the voice recording")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Recording title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the voice message caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "voice_duration"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Recording duration in seconds")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the voice recording"))
   :DOC "Represents a link to a voice recording in an .OGG container encoded with OPUS. By default, this voice recording will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the the voice message.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultDocument"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>document</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the document to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the document caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "document_url"
               :TYPE "String"
               :DESCRIPTION "A valid URL for the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mime_type"
               :TYPE "String"
               :DESCRIPTION "Mime type of the content of the file, either “application/pdf” or “application/zip”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. Inline keyboard attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. URL of the thumbnail (jpeg only) for the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail height"))
   :DOC "Represents a link to a file. By default, this file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the file. Currently, only <strong>.PDF</strong> and <strong>.ZIP</strong> files can be sent using this method.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultLocation"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>location</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 Bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "latitude"
               :TYPE "Float number"
               :DESCRIPTION "Location latitude in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "longitude"
               :TYPE "Float number"
               :DESCRIPTION "Location longitude in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Location title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "horizontal_accuracy"
               :TYPE "Float number"
               :DESCRIPTION "<em>Optional</em>. The radius of uncertainty for the location, measured in meters; 0-1500")
            #S(DATA-FIELDS-PAIRS
               :FIELD "live_period"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Period in seconds for which the location can be updated, should be between 60 and 86400.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "heading"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "proximity_alert_radius"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the location")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Url of the thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail height"))
   :DOC "Represents a location on a map. By default, the location will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the location.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultVenue"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>venue</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 Bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "latitude"
               :TYPE "Float"
               :DESCRIPTION "Latitude of the venue location in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "longitude"
               :TYPE "Float"
               :DESCRIPTION "Longitude of the venue location in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "address"
               :TYPE "String"
               :DESCRIPTION "Address of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare identifier of the venue if known")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places identifier of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places type of the venue. (See <a href=\"https://developers.google.com/places/web-service/supported_types\">supported types</a>.)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Url of the thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail height"))
   :DOC "Represents a venue. By default, the venue will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the venue.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultContact"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>contact</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 Bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "phone_number"
               :TYPE "String"
               :DESCRIPTION "Contact's phone number")
            #S(DATA-FIELDS-PAIRS
               :FIELD "first_name"
               :TYPE "String"
               :DESCRIPTION "Contact's first name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Contact's last name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "vcard"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Additional data about the contact in the form of a <a href=\"https://en.wikipedia.org/wiki/VCard\">vCard</a>, 0-2048 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the contact")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_url"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Url of the thumbnail for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_width"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail width")
            #S(DATA-FIELDS-PAIRS
               :FIELD "thumb_height"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Thumbnail height"))
   :DOC "Represents a contact with a phone number. By default, this contact will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the contact.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultGame"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>game</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "game_short_name"
               :TYPE "String"
               :DESCRIPTION "Short name of the game")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message"))
   :DOC "Represents a <a href=\"#games\">Game</a>.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedPhoto"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>photo</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier of the photo")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the photo to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the photo caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the photo"))
   :DOC "Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the photo.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedGif"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>gif</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "gif_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the GIF file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the GIF file to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the GIF animation"))
   :DOC "Represents a link to an animated GIF file stored on the Telegram servers. By default, this animated GIF file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with specified content instead of the animation.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedMpeg4Gif"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>mpeg4_gif</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "mpeg4_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the MP4 file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the MPEG-4 file to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the video animation"))
   :DOC "Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers. By default, this animated MPEG-4 file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the animation.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedSticker"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>sticker</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "sticker_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier of the sticker")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the sticker"))
   :DOC "Represents a link to a sticker stored on the Telegram servers. By default, this sticker will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the sticker.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedDocument"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>document</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "document_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the document to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the document caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the file"))
   :DOC "Represents a link to a file stored on the Telegram servers. By default, this file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the file.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedVideo"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>video</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "video_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the video file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title for the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Short description of the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption of the video to be sent, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the video caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the video"))
   :DOC "Represents a link to a video file stored on the Telegram servers. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the video.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedVoice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>voice</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "voice_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the voice message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Voice message title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the voice message caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the voice message"))
   :DOC "Represents a link to a voice message stored on the Telegram servers. By default, this voice message will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the voice message.") 
#S(API-DATATYPE
   :NAME "InlineQueryResultCachedAudio"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of the result, must be <em>audio</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this result, 1-64 bytes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "audio_file_id"
               :TYPE "String"
               :DESCRIPTION "A valid file identifier for the audio file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Caption, 0-1024 characters after entities parsing")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the audio caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "caption_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :DESCRIPTION "<em>Optional</em>. <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">Inline keyboard</a> attached to the message")
            #S(DATA-FIELDS-PAIRS
               :FIELD "input_message_content"
               :TYPE "InputMessageContent"
               :DESCRIPTION "<em>Optional</em>. Content of the message to be sent instead of the audio"))
   :DOC "Represents a link to an MP3 audio file stored on the Telegram servers. By default, this audio file will be sent by the user. Alternatively, you can use <em>input_message_content</em> to send a message with the specified content instead of the audio.") 
#S(API-DATATYPE
   :NAME "InputTextMessageContent"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "message_text"
               :TYPE "String"
               :DESCRIPTION "Text of the message to be sent, 1-4096 characters")
            #S(DATA-FIELDS-PAIRS
               :FIELD "parse_mode"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Mode for parsing entities in the message text. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. List of special entities that appear in message text, which can be specified instead of <em>parse_mode</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "disable_web_page_preview"
               :TYPE "Boolean"
               :DESCRIPTION "<em>Optional</em>. Disables link previews for links in the sent message"))
   :DOC "Represents the <a href=\"#inputmessagecontent\">content</a> of a text message to be sent as the result of an inline query.") 
#S(API-DATATYPE
   :NAME "InputLocationMessageContent"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "latitude"
               :TYPE "Float"
               :DESCRIPTION "Latitude of the location in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "longitude"
               :TYPE "Float"
               :DESCRIPTION "Longitude of the location in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "horizontal_accuracy"
               :TYPE "Float number"
               :DESCRIPTION "<em>Optional</em>. The radius of uncertainty for the location, measured in meters; 0-1500")
            #S(DATA-FIELDS-PAIRS
               :FIELD "live_period"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. Period in seconds for which the location can be updated, should be between 60 and 86400.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "heading"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "proximity_alert_radius"
               :TYPE "Integer"
               :DESCRIPTION "<em>Optional</em>. For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified."))
   :DOC "Represents the <a href=\"#inputmessagecontent\">content</a> of a location message to be sent as the result of an inline query.") 
#S(API-DATATYPE
   :NAME "InputVenueMessageContent"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "latitude"
               :TYPE "Float"
               :DESCRIPTION "Latitude of the venue in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "longitude"
               :TYPE "Float"
               :DESCRIPTION "Longitude of the venue in degrees")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Name of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "address"
               :TYPE "String"
               :DESCRIPTION "Address of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare identifier of the venue, if known")
            #S(DATA-FIELDS-PAIRS
               :FIELD "foursquare_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places identifier of the venue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "google_place_type"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Google Places type of the venue. (See <a href=\"https://developers.google.com/places/web-service/supported_types\">supported types</a>.)"))
   :DOC "Represents the <a href=\"#inputmessagecontent\">content</a> of a venue message to be sent as the result of an inline query.") 
#S(API-DATATYPE
   :NAME "InputContactMessageContent"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "phone_number"
               :TYPE "String"
               :DESCRIPTION "Contact's phone number")
            #S(DATA-FIELDS-PAIRS
               :FIELD "first_name"
               :TYPE "String"
               :DESCRIPTION "Contact's first name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "last_name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Contact's last name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "vcard"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Additional data about the contact in the form of a <a href=\"https://en.wikipedia.org/wiki/VCard\">vCard</a>, 0-2048 bytes"))
   :DOC "Represents the <a href=\"#inputmessagecontent\">content</a> of a contact message to be sent as the result of an inline query.") 
#S(API-DATATYPE
   :NAME "ChosenInlineResult"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "result_id"
               :TYPE "String"
               :DESCRIPTION "The unique identifier for the result that was chosen")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "The user that chose the result")
            #S(DATA-FIELDS-PAIRS
               :FIELD "location"
               :TYPE "Location"
               :DESCRIPTION "<em>Optional</em>. Sender location, only for bots that require user location")
            #S(DATA-FIELDS-PAIRS
               :FIELD "inline_message_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Identifier of the sent inline message. Available only if there is an <a href=\"#inlinekeyboardmarkup\">inline keyboard</a> attached to the message. Will be also received in <a href=\"#callbackquery\">callback queries</a> and can be used to <a href=\"#updating-messages\">edit</a> the message.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "query"
               :TYPE "String"
               :DESCRIPTION "The query that was used to obtain the result"))
   :DOC "Represents a <a href=\"#inlinequeryresult\">result</a> of an inline query that was chosen by the user and sent to their chat partner.") 
#S(API-DATATYPE
   :NAME "LabeledPrice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "label"
               :TYPE "String"
               :DESCRIPTION "Portion label")
            #S(DATA-FIELDS-PAIRS
               :FIELD "amount"
               :TYPE "Integer"
               :DESCRIPTION "Price of the product in the <em>smallest units</em> of the <a href=\"/bots/payments#supported-currencies\">currency</a> (integer, <strong>not</strong> float/double). For example, for a price of <code>US$ 1.45</code> pass <code>amount = 145</code>. See the <em>exp</em> parameter in <a href=\"https://core.telegram.org/bots/payments/currencies.json\">currencies.json</a>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies)."))
   :DOC "This object represents a portion of the price for goods or services.") 
#S(API-DATATYPE
   :NAME "Invoice"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Product name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "Product description")
            #S(DATA-FIELDS-PAIRS
               :FIELD "start_parameter"
               :TYPE "String"
               :DESCRIPTION "Unique bot deep-linking parameter that can be used to generate this invoice")
            #S(DATA-FIELDS-PAIRS
               :FIELD "currency"
               :TYPE "String"
               :DESCRIPTION "Three-letter ISO 4217 <a href=\"/bots/payments#supported-currencies\">currency</a> code")
            #S(DATA-FIELDS-PAIRS
               :FIELD "total_amount"
               :TYPE "Integer"
               :DESCRIPTION "Total price in the <em>smallest units</em> of the currency (integer, <strong>not</strong> float/double). For example, for a price of <code>US$ 1.45</code> pass <code>amount = 145</code>. See the <em>exp</em> parameter in <a href=\"https://core.telegram.org/bots/payments/currencies.json\">currencies.json</a>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies)."))
   :DOC "This object contains basic information about an invoice.") 
#S(API-DATATYPE
   :NAME "ShippingAddress"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "country_code"
               :TYPE "String"
               :DESCRIPTION "ISO 3166-1 alpha-2 country code")
            #S(DATA-FIELDS-PAIRS
               :FIELD "state"
               :TYPE "String"
               :DESCRIPTION "State, if applicable")
            #S(DATA-FIELDS-PAIRS
               :FIELD "city"
               :TYPE "String"
               :DESCRIPTION "City")
            #S(DATA-FIELDS-PAIRS
               :FIELD "street_line1"
               :TYPE "String"
               :DESCRIPTION "First line for the address")
            #S(DATA-FIELDS-PAIRS
               :FIELD "street_line2"
               :TYPE "String"
               :DESCRIPTION "Second line for the address")
            #S(DATA-FIELDS-PAIRS
               :FIELD "post_code"
               :TYPE "String"
               :DESCRIPTION "Address post code"))
   :DOC "This object represents a shipping address.") 
#S(API-DATATYPE
   :NAME "OrderInfo"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "name"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User name")
            #S(DATA-FIELDS-PAIRS
               :FIELD "phone_number"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User's phone number")
            #S(DATA-FIELDS-PAIRS
               :FIELD "email"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User email")
            #S(DATA-FIELDS-PAIRS
               :FIELD "shipping_address"
               :TYPE "ShippingAddress"
               :DESCRIPTION "<em>Optional</em>. User shipping address"))
   :DOC "This object represents information about an order.") 
#S(API-DATATYPE
   :NAME "ShippingOption"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Shipping option identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Option title")
            #S(DATA-FIELDS-PAIRS
               :FIELD "prices"
               :TYPE "LabeledPrice"
               :DESCRIPTION "List of price portions"))
   :DOC "This object represents one shipping option.") 
#S(API-DATATYPE
   :NAME "SuccessfulPayment"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "currency"
               :TYPE "String"
               :DESCRIPTION "Three-letter ISO 4217 <a href=\"/bots/payments#supported-currencies\">currency</a> code")
            #S(DATA-FIELDS-PAIRS
               :FIELD "total_amount"
               :TYPE "Integer"
               :DESCRIPTION "Total price in the <em>smallest units</em> of the currency (integer, <strong>not</strong> float/double). For example, for a price of <code>US$ 1.45</code> pass <code>amount = 145</code>. See the <em>exp</em> parameter in <a href=\"https://core.telegram.org/bots/payments/currencies.json\">currencies.json</a>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).")
            #S(DATA-FIELDS-PAIRS
               :FIELD "invoice_payload"
               :TYPE "String"
               :DESCRIPTION "Bot specified invoice payload")
            #S(DATA-FIELDS-PAIRS
               :FIELD "shipping_option_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Identifier of the shipping option chosen by the user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "order_info"
               :TYPE "OrderInfo"
               :DESCRIPTION "<em>Optional</em>. Order info provided by the user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "telegram_payment_charge_id"
               :TYPE "String"
               :DESCRIPTION "Telegram payment identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "provider_payment_charge_id"
               :TYPE "String"
               :DESCRIPTION "Provider payment identifier"))
   :DOC "This object contains basic information about a successful payment.") 
#S(API-DATATYPE
   :NAME "ShippingQuery"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique query identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "User who sent the query")
            #S(DATA-FIELDS-PAIRS
               :FIELD "invoice_payload"
               :TYPE "String"
               :DESCRIPTION "Bot specified invoice payload")
            #S(DATA-FIELDS-PAIRS
               :FIELD "shipping_address"
               :TYPE "ShippingAddress"
               :DESCRIPTION "User specified shipping address"))
   :DOC "This object contains information about an incoming shipping query.") 
#S(API-DATATYPE
   :NAME "PreCheckoutQuery"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "id"
               :TYPE "String"
               :DESCRIPTION "Unique query identifier")
            #S(DATA-FIELDS-PAIRS
               :FIELD "from"
               :TYPE "User"
               :DESCRIPTION "User who sent the query")
            #S(DATA-FIELDS-PAIRS
               :FIELD "currency"
               :TYPE "String"
               :DESCRIPTION "Three-letter ISO 4217 <a href=\"/bots/payments#supported-currencies\">currency</a> code")
            #S(DATA-FIELDS-PAIRS
               :FIELD "total_amount"
               :TYPE "Integer"
               :DESCRIPTION "Total price in the <em>smallest units</em> of the currency (integer, <strong>not</strong> float/double). For example, for a price of <code>US$ 1.45</code> pass <code>amount = 145</code>. See the <em>exp</em> parameter in <a href=\"https://core.telegram.org/bots/payments/currencies.json\">currencies.json</a>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).")
            #S(DATA-FIELDS-PAIRS
               :FIELD "invoice_payload"
               :TYPE "String"
               :DESCRIPTION "Bot specified invoice payload")
            #S(DATA-FIELDS-PAIRS
               :FIELD "shipping_option_id"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Identifier of the shipping option chosen by the user")
            #S(DATA-FIELDS-PAIRS
               :FIELD "order_info"
               :TYPE "OrderInfo"
               :DESCRIPTION "<em>Optional</em>. Order info provided by the user"))
   :DOC "This object contains information about an incoming pre-checkout query.") 
#S(API-DATATYPE
   :NAME "PassportData"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "data"
               :TYPE "EncryptedPassportElement"
               :DESCRIPTION "Array with information about documents and other Telegram Passport elements that was shared with the bot")
            #S(DATA-FIELDS-PAIRS
               :FIELD "credentials"
               :TYPE "EncryptedCredentials"
               :DESCRIPTION "Encrypted credentials required to decrypt the data"))
   :DOC "Contains information about Telegram Passport data shared with the bot by the user.") 
#S(API-DATATYPE
   :NAME "PassportFile"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "file_id"
               :TYPE "String"
               :DESCRIPTION "Identifier for this file, which can be used to download or reuse the file")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_unique_id"
               :TYPE "String"
               :DESCRIPTION "Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_size"
               :TYPE "Integer"
               :DESCRIPTION "File size")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_date"
               :TYPE "Integer"
               :DESCRIPTION "Unix time when the file was uploaded"))
   :DOC "This object represents a file uploaded to Telegram Passport. Currently all Telegram Passport files are in JPEG format when decrypted and don't exceed 10MB.") 
#S(API-DATATYPE
   :NAME "EncryptedPassportElement"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Element type. One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "data"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Base64-encoded encrypted Telegram Passport element data provided by the user, available for “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport” and “address” types. Can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "phone_number"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User's verified phone number, available only for “phone_number” type")
            #S(DATA-FIELDS-PAIRS
               :FIELD "email"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. User's verified email address, available only for “email” type")
            #S(DATA-FIELDS-PAIRS
               :FIELD "files"
               :TYPE "PassportFile"
               :DESCRIPTION "<em>Optional</em>. Array of encrypted files with documents provided by the user, available for “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "front_side"
               :TYPE "PassportFile"
               :DESCRIPTION "<em>Optional</em>. Encrypted file with the front side of the document, provided by the user. Available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "reverse_side"
               :TYPE "PassportFile"
               :DESCRIPTION "<em>Optional</em>. Encrypted file with the reverse side of the document, provided by the user. Available for “driver_license” and “identity_card”. The file can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "selfie"
               :TYPE "PassportFile"
               :DESCRIPTION "<em>Optional</em>. Encrypted file with the selfie of the user holding a document, provided by the user; available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "translation"
               :TYPE "PassportFile"
               :DESCRIPTION "<em>Optional</em>. Array of encrypted files with translated versions of documents provided by the user. Available if requested for “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying <a href=\"#encryptedcredentials\">EncryptedCredentials</a>.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded element hash for using in <a href=\"#passportelementerrorunspecified\">PassportElementErrorUnspecified</a>"))
   :DOC "Contains information about documents or other Telegram Passport elements shared with the bot by the user.") 
#S(API-DATATYPE
   :NAME "EncryptedCredentials"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "data"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded encrypted JSON-serialized data with unique user's payload, data hashes and secrets required for <a href=\"#encryptedpassportelement\">EncryptedPassportElement</a> decryption and authentication")
            #S(DATA-FIELDS-PAIRS
               :FIELD "hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded data hash for data authentication")
            #S(DATA-FIELDS-PAIRS
               :FIELD "secret"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded secret, encrypted with the bot's public RSA key, required for data decryption"))
   :DOC "Contains data required for decrypting and authenticating <a href=\"#encryptedpassportelement\">EncryptedPassportElement</a>. See the <a href=\"https://core.telegram.org/passport#receiving-information\">Telegram Passport Documentation</a> for a complete description of the data decryption and authentication processes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorDataField"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>data</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the error, one of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "field_name"
               :TYPE "String"
               :DESCRIPTION "Name of the data field which has the error")
            #S(DATA-FIELDS-PAIRS
               :FIELD "data_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded data hash")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue in one of the data fields that was provided by the user. The error is considered resolved when the field's value changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorFrontSide"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>front_side</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded hash of the file with the front side of the document")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with the front side of a document. The error is considered resolved when the file with the front side of the document changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorReverseSide"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>reverse_side</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the issue, one of “driver_license”, “identity_card”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded hash of the file with the reverse side of the document")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with the reverse side of a document. The error is considered resolved when the file with reverse side of the document changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorSelfie"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>selfie</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded hash of the file with the selfie")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with the selfie with a document. The error is considered resolved when the file with the selfie changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorFile"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>file</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the issue, one of “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded file hash")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with a document scan. The error is considered resolved when the file with the document scan changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorFiles"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>files</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "The section of the user's Telegram Passport which has the issue, one of “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hashes"
               :TYPE "Array of String"
               :DESCRIPTION "List of base64-encoded file hashes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with a list of scans. The error is considered resolved when the list of files containing the scans changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorTranslationFile"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>translation_file</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of element of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded file hash")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with one of the files that constitute the translation of a document. The error is considered resolved when the file changes.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorTranslationFiles"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>translation_files</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of element of the user's Telegram Passport which has the issue, one of “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”")
            #S(DATA-FIELDS-PAIRS
               :FIELD "file_hashes"
               :TYPE "Array of String"
               :DESCRIPTION "List of base64-encoded file hashes")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue with the translated version of a document. The error is considered resolved when a file with the document translation change.") 
#S(API-DATATYPE
   :NAME "PassportElementErrorUnspecified"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "source"
               :TYPE "String"
               :DESCRIPTION "Error source, must be <em>unspecified</em>")
            #S(DATA-FIELDS-PAIRS
               :FIELD "type"
               :TYPE "String"
               :DESCRIPTION "Type of element of the user's Telegram Passport which has the issue")
            #S(DATA-FIELDS-PAIRS
               :FIELD "element_hash"
               :TYPE "String"
               :DESCRIPTION "Base64-encoded element hash")
            #S(DATA-FIELDS-PAIRS
               :FIELD "message"
               :TYPE "String"
               :DESCRIPTION "Error message"))
   :DOC "Represents an issue in an unspecified place. The error is considered resolved when new data is added.") 
#S(API-DATATYPE
   :NAME "Game"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "title"
               :TYPE "String"
               :DESCRIPTION "Title of the game")
            #S(DATA-FIELDS-PAIRS
               :FIELD "description"
               :TYPE "String"
               :DESCRIPTION "Description of the game")
            #S(DATA-FIELDS-PAIRS
               :FIELD "photo"
               :TYPE "PhotoSize"
               :DESCRIPTION "Photo that will be displayed in the game message in chats.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "text"
               :TYPE "String"
               :DESCRIPTION "<em>Optional</em>. Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls <a href=\"#setgamescore\">setGameScore</a>, or manually edited using <a href=\"#editmessagetext\">editMessageText</a>. 0-4096 characters.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "text_entities"
               :TYPE "MessageEntity"
               :DESCRIPTION "<em>Optional</em>. Special entities that appear in <em>text</em>, such as usernames, URLs, bot commands, etc.")
            #S(DATA-FIELDS-PAIRS
               :FIELD "animation"
               :TYPE "Animation"
               :DESCRIPTION "<em>Optional</em>. Animation that will be displayed in the game message in chats. Upload via <a href=\"https://t.me/botfather\">BotFather</a>"))
   :DOC "This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.") 
#S(API-DATATYPE
   :NAME "GameHighScore"
   :FIELDS (#S(DATA-FIELDS-PAIRS
               :FIELD "position"
               :TYPE "Integer"
               :DESCRIPTION "Position in high score table for the game")
            #S(DATA-FIELDS-PAIRS
               :FIELD "user"
               :TYPE "User"
               :DESCRIPTION "User")
            #S(DATA-FIELDS-PAIRS
               :FIELD "score"
               :TYPE "Integer"
               :DESCRIPTION "Score"))
   :DOC "This object represents one row of the high scores table for a game.") 
))
(defparameter *all-methods-static* '(#S(API-METHOD
                                        :NAME "getUpdates"
                                        :FIELDS (#S(METHOD-FIELDS-PAIRS
                                                    :PARAMETER "offset"
                                                    :TYPE "Integer"
                                                    :REQUIRED "Optional"
                                                    :DESCRIPTION "Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as <a href=\"#getupdates\">getUpdates</a> is called with an <em>offset</em> higher than its <em>update_id</em>. The negative offset can be specified to retrieve updates starting from <em>-offset</em> update from the end of the updates queue. All previous updates will forgotten.")
                                                 #S(METHOD-FIELDS-PAIRS
                                                    :PARAMETER "limit"
                                                    :TYPE "Integer"
                                                    :REQUIRED "Optional"
                                                    :DESCRIPTION "Limits the number of updates to be retrieved. Values between 1-100 are accepted. Defaults to 100.")
                                                 #S(METHOD-FIELDS-PAIRS
                                                    :PARAMETER "timeout"
                                                    :TYPE "Integer"
                                                    :REQUIRED "Optional"
                                                    :DESCRIPTION "Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.")
                                                 #S(METHOD-FIELDS-PAIRS
                                                    :PARAMETER "allowed_updates"
                                                    :TYPE "Array of String"
                                                    :REQUIRED "Optional"
                                                    :DESCRIPTION "A JSON-serialized list of the update types you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See <a href=\"#update\">Update</a> for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used.<br><br>Please note that this parameter doesn't affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time."))
                                        :DOC "Use this method to receive incoming updates using long polling (<a href=\"https://en.wikipedia.org/wiki/Push_technology#Long_polling\">wiki</a>). An Array of <a href=\"#update\">Update</a> objects is returned.") 
#S(API-METHOD
   :NAME "setWebhook"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "url"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "HTTPS url to send updates to. Use an empty string to remove webhook integration")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "certificate"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Upload your public key certificate so that the root certificate in use can be checked. See our <a href=\"/bots/self-signed\">self-signed guide</a> for details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "ip_address"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "The fixed IP address which will be used to send webhook requests instead of the IP address resolved through DNS")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "max_connections"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery, 1-100. Defaults to <em>40</em>. Use lower values to limit the load on your bot's server, and higher values to increase your bot's throughput.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allowed_updates"
               :TYPE "Array of String"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized list of the update types you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See <a href=\"#update\">Update</a> for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used.<br>Please note that this parameter doesn't affect updates created before the call to the setWebhook, so unwanted updates may be received for a short period of time.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "drop_pending_updates"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em> to drop all pending updates"))
   :DOC "If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. <code>https://www.example.com/&lt;token&gt;</code>. Since nobody else knows your bot's token, you can be pretty sure it's us.") 
#S(API-METHOD
   :NAME "deleteWebhook"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "drop_pending_updates"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em> to drop all pending updates"))
   :DOC "Use this method to remove webhook integration if you decide to switch back to <a href=\"#getupdates\">getUpdates</a>. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "sendMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "text"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Text of the message to be sent, 1-4096 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the message text. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in message text, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_web_page_preview"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Disables link previews for links in this message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send text messages. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "forwardMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "from_chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the chat where the original message was sent (or channel username in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Message identifier in the chat specified in <em>from_chat_id</em>"))
   :DOC "Use this method to forward messages of any kind. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "copyMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "from_chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the chat where the original message was sent (or channel username in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Message identifier in the chat specified in <em>from_chat_id</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "New caption for media, 0-1024 characters after entities parsing. If not specified, the original caption is kept")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the new caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the new caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to copy messages of any kind. The method is analogous to the method <a href=\"#forwardmessages\">forwardMessages</a>, but the copied message doesn't have a link to the original message. Returns the <a href=\"#messageid\">MessageId</a> of the sent message on success.") 
#S(API-METHOD
   :NAME "sendPhoto"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Photo to send. Pass a file_id as String to send a photo that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a photo from the Internet, or upload a new photo using multipart/form-data. The photo must be at most 10 MB in size. The photo's width and height must not exceed 10000 in total. Width and height ratio must be at most 20. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Photo caption (may also be used when resending photos by <em>file_id</em>), 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the photo caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send photos. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendAudio"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "audio"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Audio file to send. Pass a file_id as String to send an audio file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an audio file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Audio caption, 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the audio caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "duration"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Duration of the audio in seconds")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "performer"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Performer")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "title"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Track name")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "For sending voice messages, use the <a href=\"#sendvoice\">sendVoice</a> method instead.") 
#S(API-METHOD
   :NAME "sendDocument"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "document"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "File to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Document caption (may also be used when resending documents by <em>file_id</em>), 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the document caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_content_type_detection"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Disables automatic server-side content type detection for files uploaded using multipart/form-data")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send general files. On success, the sent <a href=\"#message\">Message</a> is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.") 
#S(API-METHOD
   :NAME "sendVideo"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "video"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Video to send. Pass a file_id as String to send a video that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a video from the Internet, or upload a new video using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "duration"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Duration of sent video in seconds")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "width"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Video width")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "height"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Video height")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Video caption (may also be used when resending videos by <em>file_id</em>), 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the video caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "supports_streaming"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the uploaded video is suitable for streaming")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send video files, Telegram clients support mp4 videos (other formats may be sent as <a href=\"#document\">Document</a>). On success, the sent <a href=\"#message\">Message</a> is returned. Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.") 
#S(API-METHOD
   :NAME "sendAnimation"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "animation"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Animation to send. Pass a file_id as String to send an animation that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an animation from the Internet, or upload a new animation using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "duration"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Duration of sent animation in seconds")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "width"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Animation width")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "height"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Animation height")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Animation caption (may also be used when resending animation by <em>file_id</em>), 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the animation caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send animation files (GIF or H.264/MPEG-4 AVC video without sound). On success, the sent <a href=\"#message\">Message</a> is returned. Bots can currently send animation files of up to 50 MB in size, this limit may be changed in the future.") 
#S(API-METHOD
   :NAME "sendVoice"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "voice"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Audio file to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Voice message caption, 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the voice message caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "duration"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Duration of the voice message in seconds")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send audio files, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .OGG file encoded with OPUS (other formats may be sent as <a href=\"#audio\">Audio</a> or <a href=\"#document\">Document</a>). On success, the sent <a href=\"#message\">Message</a> is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.") 
#S(API-METHOD
   :NAME "sendVideoNote"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "video_note"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Video note to send. Pass a file_id as String to send a video note that exists on the Telegram servers (recommended) or upload a new video using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>. Sending video notes by a URL is currently unsupported")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "duration"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Duration of sent video in seconds")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "length"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Video width and height, i.e. diameter of the video message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://&lt;file_attach_name&gt;” if the thumbnail was uploaded using multipart/form-data under &lt;file_attach_name&gt;. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "As of <a href=\"https://telegram.org/blog/video-messages-and-telescope\">v.4.0</a>, Telegram clients support rounded square mp4 videos of up to 1 minute long. Use this method to send video messages. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendMediaGroup"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "media"
               :TYPE "InputMediaAudio"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized array describing messages to be sent, must include 2-10 items")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends messages <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the messages are a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found"))
   :DOC "Use this method to send a group of photos, videos, documents or audios as an album. Documents and audio files can be only grouped in an album with messages of the same type. On success, an array of <a href=\"#message\">Messages</a> that were sent is returned.") 
#S(API-METHOD
   :NAME "sendLocation"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "latitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Latitude of the location")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "longitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Longitude of the location")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "horizontal_accuracy"
               :TYPE "Float number"
               :REQUIRED "Optional"
               :DESCRIPTION "The radius of uncertainty for the location, measured in meters; 0-1500")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "live_period"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Period in seconds for which the location will be updated (see <a href=\"https://telegram.org/blog/live-locations\">Live Locations</a>, should be between 60 and 86400.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "heading"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "proximity_alert_radius"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send point on the map. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "editMessageLiveLocation"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message to edit")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "latitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Latitude of new location")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "longitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Longitude of new location")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "horizontal_accuracy"
               :TYPE "Float number"
               :REQUIRED "Optional"
               :DESCRIPTION "The radius of uncertainty for the location, measured in meters; 0-1500")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "heading"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "proximity_alert_radius"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for a new <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to edit live location messages. A location can be edited until its <em>live_period</em> expires or editing is explicitly disabled by a call to <a href=\"#stopmessagelivelocation\">stopMessageLiveLocation</a>. On success, if the edited message is not an inline message, the edited <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "stopMessageLiveLocation"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message with live location to stop")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for a new <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to stop updating a live location message before <em>live_period</em> expires. On success, if the message was sent by the bot, the sent <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "sendVenue"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "latitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Latitude of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "longitude"
               :TYPE "Float number"
               :REQUIRED "Yes"
               :DESCRIPTION "Longitude of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "title"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Name of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "address"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Address of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "foursquare_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Foursquare identifier of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "foursquare_type"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "google_place_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Google Places identifier of the venue")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "google_place_type"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Google Places type of the venue. (See <a href=\"https://developers.google.com/places/web-service/supported_types\">supported types</a>.)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send information about a venue. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendContact"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "phone_number"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Contact's phone number")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "first_name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Contact's first name")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "last_name"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Contact's last name")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "vcard"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional data about the contact in the form of a <a href=\"https://en.wikipedia.org/wiki/VCard\">vCard</a>, 0-2048 bytes")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove keyboard or to force a reply from the user."))
   :DOC "Use this method to send phone contacts. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendPoll"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "question"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Poll question, 1-300 characters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "options"
               :TYPE "Array of String"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized list of answer options, 2-10 strings 1-100 characters each")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "is_anonymous"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "True, if the poll needs to be anonymous, defaults to <em>True</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "type"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Poll type, “quiz” or “regular”, defaults to “regular”")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allows_multiple_answers"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "True, if the poll allows multiple answers, ignored for polls in quiz mode, defaults to <em>False</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "correct_option_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "0-based identifier of the correct answer option, required for polls in quiz mode")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "explanation"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters with at most 2 line feeds after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "explanation_parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the explanation. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "explanation_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the poll explanation, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "open_period"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Amount of time in seconds the poll will be active after creation, 5-600. Can't be used together with <em>close_date</em>.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "close_date"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Point in time (Unix timestamp) when the poll will be automatically closed. Must be at least 5 and no more than 600 seconds in the future. Can't be used together with <em>open_period</em>.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "is_closed"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the poll needs to be immediately closed. This can be useful for poll preview.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send a native poll. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendDice"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "emoji"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Emoji on which the dice throw animation is based. Currently, must be one of “<img height=\"20\" src=\"//telegram.org/img/emoji/40/F09F8EB2.png\" class=\"emoji\" alt=\"🎲\" width=\"20\">”, “<img src=\"//telegram.org/img/emoji/40/F09F8EAF.png\" width=\"20\" alt=\"🎯\" class=\"emoji\" height=\"20\">”, “<img width=\"20\" src=\"//telegram.org/img/emoji/40/F09F8F80.png\" alt=\"🏀\" height=\"20\" class=\"emoji\">”, “<img src=\"//telegram.org/img/emoji/40/E29ABD.png\" class=\"emoji\" width=\"20\" height=\"20\" alt=\"⚽\">”, or “<img src=\"//telegram.org/img/emoji/40/F09F8EB0.png\" height=\"20\" width=\"20\" alt=\"🎰\" class=\"emoji\">”. Dice can have values 1-6 for “<img class=\"emoji\" src=\"//telegram.org/img/emoji/40/F09F8EB2.png\" width=\"20\" height=\"20\" alt=\"🎲\">” and “<img width=\"20\" class=\"emoji\" src=\"//telegram.org/img/emoji/40/F09F8EAF.png\" alt=\"🎯\" height=\"20\">”, values 1-5 for “<img width=\"20\" class=\"emoji\" src=\"//telegram.org/img/emoji/40/F09F8F80.png\" height=\"20\" alt=\"🏀\">” and “<img height=\"20\" alt=\"⚽\" class=\"emoji\" width=\"20\" src=\"//telegram.org/img/emoji/40/E29ABD.png\">”, and values 1-64 for “<img alt=\"🎰\" class=\"emoji\" src=\"//telegram.org/img/emoji/40/F09F8EB0.png\" height=\"20\" width=\"20\">”. Defaults to “<img width=\"20\" alt=\"🎲\" class=\"emoji\" src=\"//telegram.org/img/emoji/40/F09F8EB2.png\" height=\"20\">”")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send an animated emoji that will display a random value. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "sendChatAction"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "action"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Type of action to broadcast. Choose one, depending on what the user is about to receive: <em>typing</em> for <a href=\"#sendmessage\">text messages</a>, <em>upload_photo</em> for <a href=\"#sendphoto\">photos</a>, <em>record_video</em> or <em>upload_video</em> for <a href=\"#sendvideo\">videos</a>, <em>record_voice</em> or <em>upload_voice</em> for <a href=\"#sendvoice\">voice notes</a>, <em>upload_document</em> for <a href=\"#senddocument\">general files</a>, <em>find_location</em> for <a href=\"#sendlocation\">location data</a>, <em>record_video_note</em> or <em>upload_video_note</em> for <a href=\"#sendvideonote\">video notes</a>."))
   :DOC "We only recommend using this method when a response from the bot will take a <strong>noticeable</strong> amount of time to arrive.") 
#S(API-METHOD
   :NAME "getUserProfilePhotos"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "offset"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Sequential number of the first photo to be returned. By default, all photos are returned.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "limit"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Limits the number of photos to be retrieved. Values between 1-100 are accepted. Defaults to 100."))
   :DOC "Use this method to get a list of profile pictures for a user. Returns a <a href=\"#userprofilephotos\">UserProfilePhotos</a> object.") 
#S(API-METHOD
   :NAME "getFile"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "file_id"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "File identifier to get info about"))
   :DOC "Use this method to get basic info about a file and prepare it for downloading. For the moment, bots can download files of up to 20MB in size. On success, a <a href=\"#file\">File</a> object is returned. The file can then be downloaded via the link <code>https://api.telegram.org/file/bot&lt;token&gt;/&lt;file_path&gt;</code>, where <code>&lt;file_path&gt;</code> is taken from the response. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling <a href=\"#getfile\">getFile</a> again.") 
#S(API-METHOD
   :NAME "kickChatMember"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target group or username of the target supergroup or channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "until_date"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Date when the user will be unbanned, unix time. If user is banned for more than 366 days or less than 30 seconds from the current time they are considered to be banned forever"))
   :DOC "Use this method to kick a user from a group, a supergroup or a channel. In the case of supergroups and channels, the user will not be able to return to the group on their own using invite links, etc., unless <a href=\"#unbanchatmember\">unbanned</a> first. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "unbanChatMember"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target group or username of the target supergroup or channel (in the format <code>@username</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "only_if_banned"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Do nothing if the user is not banned"))
   :DOC "Use this method to unban a previously kicked user in a supergroup or channel. The user will <strong>not</strong> return to the group or channel automatically, but will be able to join via link, etc. The bot must be an administrator for this to work. By default, this method guarantees that after the call the user is not a member of the chat, but will be able to join it. So if the user is a member of the chat they will also be <strong>removed</strong> from the chat. If you don't want this, use the parameter <em>only_if_banned</em>. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "restrictChatMember"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup (in the format <code>@supergroupusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "permissions"
               :TYPE "ChatPermissions"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized object for new user permissions")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "until_date"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Date when restrictions will be lifted for the user, unix time. If user is restricted for more than 366 days or less than 30 seconds from the current time, they are considered to be restricted forever"))
   :DOC "Use this method to restrict a user in a supergroup. The bot must be an administrator in the supergroup for this to work and must have the appropriate admin rights. Pass <em>True</em> for all permissions to lift restrictions from a user. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "promoteChatMember"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "is_anonymous"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the administrator's presence in the chat is hidden")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_change_info"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can change chat title, photo and other settings")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_post_messages"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can create channel posts, channels only")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_edit_messages"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can edit messages of other users and can pin messages, channels only")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_delete_messages"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can delete messages of other users")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_invite_users"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can invite new users to the chat")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_restrict_members"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can restrict, ban or unban chat members")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_pin_messages"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can pin messages, supergroups only")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "can_promote_members"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by him)"))
   :DOC "Use this method to promote or demote a user in a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Pass <em>False</em> for all boolean parameters to demote a user. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setChatAdministratorCustomTitle"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup (in the format <code>@supergroupusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "custom_title"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "New custom title for the administrator; 0-16 characters, emoji are not allowed"))
   :DOC "Use this method to set a custom title for an administrator in a supergroup promoted by the bot. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setChatPermissions"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup (in the format <code>@supergroupusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "permissions"
               :TYPE "ChatPermissions"
               :REQUIRED "Yes"
               :DESCRIPTION "New default chat permissions"))
   :DOC "Use this method to set default chat permissions for all members. The bot must be an administrator in the group or a supergroup for this to work and must have the <em>can_restrict_members</em> admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "exportChatInviteLink"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to generate a new invite link for a chat; any previously generated link is revoked. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns the new invite link as <em>String</em> on success.") 
#S(API-METHOD
   :NAME "setChatPhoto"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "New chat photo, uploaded using multipart/form-data"))
   :DOC "Use this method to set a new profile photo for the chat. Photos can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "deleteChatPhoto"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to delete a chat photo. Photos can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setChatTitle"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "title"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "New chat title, 1-255 characters"))
   :DOC "Use this method to change the title of a chat. Titles can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setChatDescription"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "description"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "New chat description, 0-255 characters"))
   :DOC "Use this method to change the description of a group, a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "pinChatMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Identifier of a message to pin")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if it is not necessary to send a notification to all chat members about the new pinned message. Notifications are always disabled in channels and private chats."))
   :DOC "Use this method to add a message to the list of pinned messages in a chat. If the chat is not a private chat, the bot must be an administrator in the chat for this to work and must have the 'can_pin_messages' admin right in a supergroup or 'can_edit_messages' admin right in a channel. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "unpinChatMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Identifier of a message to unpin. If not specified, the most recent pinned message (by sending date) will be unpinned."))
   :DOC "Use this method to remove a message from the list of pinned messages in a chat. If the chat is not a private chat, the bot must be an administrator in the chat for this to work and must have the 'can_pin_messages' admin right in a supergroup or 'can_edit_messages' admin right in a channel. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "unpinAllChatMessages"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to clear the list of pinned messages in a chat. If the chat is not a private chat, the bot must be an administrator in the chat for this to work and must have the 'can_pin_messages' admin right in a supergroup or 'can_edit_messages' admin right in a channel. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "leaveChat"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup or channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method for your bot to leave a group, supergroup or channel. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "getChat"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup or channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.). Returns a <a href=\"#chat\">Chat</a> object on success.") 
#S(API-METHOD
   :NAME "getChatAdministrators"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup or channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to get a list of administrators in a chat. On success, returns an Array of <a href=\"#chatmember\">ChatMember</a> objects that contains information about all chat administrators except other bots. If the chat is a group or a supergroup and no administrators were appointed, only the creator will be returned.") 
#S(API-METHOD
   :NAME "getChatMembersCount"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup or channel (in the format <code>@channelusername</code>)"))
   :DOC "Use this method to get the number of members in a chat. Returns <em>Int</em> on success.") 
#S(API-METHOD
   :NAME "getChatMember"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup or channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier of the target user"))
   :DOC "Use this method to get information about a member of a chat. Returns a <a href=\"#chatmember\">ChatMember</a> object on success.") 
#S(API-METHOD
   :NAME "setChatStickerSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup (in the format <code>@supergroupusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "sticker_set_name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Name of the sticker set to be set as the group sticker set"))
   :DOC "Use this method to set a new group sticker set for a supergroup. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Use the field <em>can_set_sticker_set</em> optionally returned in <a href=\"#getchat\">getChat</a> requests to check if the bot can use this method. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "deleteChatStickerSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target supergroup (in the format <code>@supergroupusername</code>)"))
   :DOC "Use this method to delete a group sticker set from a supergroup. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Use the field <em>can_set_sticker_set</em> optionally returned in <a href=\"#getchat\">getChat</a> requests to check if the bot can use this method. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "answerCallbackQuery"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "callback_query_id"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the query to be answered")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "text"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "show_alert"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "If <em>true</em>, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to <em>false</em>.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "url"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "URL that will be opened by the user's client. If you have created a <a href=\"#game\">Game</a> and accepted the conditions via <a href=\"https://t.me/botfather\">@Botfather</a>, specify the URL that opens your game — note that this will only work if the query comes from a <a href=\"#inlinekeyboardbutton\"><em>callback_game</em></a> button.<br><br>Otherwise, you may use links like <code>t.me/your_bot?start=XXXX</code> that open your bot with a parameter.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "cache_time"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0."))
   :DOC "Use this method to send answers to callback queries sent from <a href=\"/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboards</a>. The answer will be displayed to the user as a notification at the top of the chat screen or as an alert. On success, <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "setMyCommands"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "commands"
               :TYPE "BotCommand"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized list of bot commands to be set as the list of the bot's commands. At most 100 commands can be specified."))
   :DOC "Use this method to change the list of the bot's commands. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "editMessageText"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message to edit")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "text"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "New text of the message, 1-4096 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the message text. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in message text, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_web_page_preview"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Disables link previews for links in this message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to edit text and <a href=\"#games\">game</a> messages. On success, if the edited message is not an inline message, the edited <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "editMessageCaption"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message to edit")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "New caption of the message, 0-1024 characters after entities parsing")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "parse_mode"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Mode for parsing entities in the message caption. See <a href=\"#formatting-options\">formatting options</a> for more details.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "caption_entities"
               :TYPE "MessageEntity"
               :REQUIRED "Optional"
               :DESCRIPTION "List of special entities that appear in the caption, which can be specified instead of <em>parse_mode</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to edit captions of messages. On success, if the edited message is not an inline message, the edited <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "editMessageMedia"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message to edit")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "media"
               :TYPE "InputMedia"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized object for a new media content of the message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for a new <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to edit animation, audio, document, photo, or video messages. If a message is part of a message album, then it can be edited only to an audio for audio albums, only to a document for document albums and to a photo or a video otherwise. When an inline message is edited, a new file can't be uploaded. Use a previously uploaded file via its file_id or specify a URL. On success, if the edited message was sent by the bot, the edited <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "editMessageReplyMarkup"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the message to edit")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to edit only the reply markup of messages. On success, if the edited message is not an inline message, the edited <a href=\"#message\">Message</a> is returned, otherwise <em>True</em> is returned.") 
#S(API-METHOD
   :NAME "stopPoll"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Identifier of the original message with the poll")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for a new message <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>."))
   :DOC "Use this method to stop a poll which was sent by the bot. On success, the stopped <a href=\"#poll\">Poll</a> with the final results is returned.") 
#S(API-METHOD
   :NAME "deleteMessage"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Identifier of the message to delete"))
   :DOC "Use this method to delete a message, including service messages, with the following limitations:<br>- A message can only be deleted if it was sent less than 48 hours ago.<br>- A dice message in a private chat can only be deleted if it was sent more than 24 hours ago.<br>- Bots can delete outgoing messages in private chats, groups, and supergroups.<br>- Bots can delete incoming messages in private chats.<br>- Bots granted <em>can_post_messages</em> permissions can delete outgoing messages in channels.<br>- If the bot is an administrator of a group, it can delete any message there.<br>- If the bot has <em>can_delete_messages</em> permission in a supergroup or a channel, it can delete any message there.<br>Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "sendSticker"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer or String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat or username of the target channel (in the format <code>@channelusername</code>)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "sticker"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "Sticker to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a .WEBP file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "Additional interface options. A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>, <a href=\"https://core.telegram.org/bots#keyboards\">custom reply keyboard</a>, instructions to remove reply keyboard or to force a reply from the user."))
   :DOC "Use this method to send static .WEBP or <a href=\"https://telegram.org/blog/animated-stickers\">animated</a> .TGS stickers. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "getStickerSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Name of the sticker set"))
   :DOC "Use this method to get a sticker set. On success, a <a href=\"#stickerset\">StickerSet</a> object is returned.") 
#S(API-METHOD
   :NAME "uploadStickerFile"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier of sticker file owner")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "png_sticker"
               :TYPE "InputFile"
               :REQUIRED "Yes"
               :DESCRIPTION "<strong>PNG</strong> image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. <a href=\"#sending-files\">More info on Sending Files »</a>"))
   :DOC "Use this method to upload a .PNG file with a sticker for later use in <em>createNewStickerSet</em> and <em>addStickerToSet</em> methods (can be used multiple times). Returns the uploaded <a href=\"#file\">File</a> on success.") 
#S(API-METHOD
   :NAME "createNewStickerSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier of created sticker set owner")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Short name of sticker set, to be used in <code>t.me/addstickers/</code> URLs (e.g., <em>animals</em>). Can contain only english letters, digits and underscores. Must begin with a letter, can't contain consecutive underscores and must end in <em>“_by_&lt;bot username&gt;”</em>. <em>&lt;bot_username&gt;</em> is case insensitive. 1-64 characters.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "title"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Sticker set title, 1-64 characters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "png_sticker"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "<strong>PNG</strong> image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a <em>file_id</em> as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "tgs_sticker"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "<strong>TGS</strong> animation with the sticker, uploaded using multipart/form-data. See <a href=\"https://core.telegram.org/animated_stickers#technical-requirements\"></a><a href=\"https://core.telegram.org/animated_stickers#technical-requirements\">https://core.telegram.org/animated_stickers#technical-requirements</a> for technical requirements")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "emojis"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "One or more emoji corresponding to the sticker")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "contains_masks"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if a set of mask stickers should be created")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "mask_position"
               :TYPE "MaskPosition"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for position where the mask should be placed on faces"))
   :DOC "Use this method to create a new sticker set owned by a user. The bot will be able to edit the sticker set thus created. You <strong>must</strong> use exactly one of the fields <em>png_sticker</em> or <em>tgs_sticker</em>. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "addStickerToSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier of sticker set owner")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Sticker set name")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "png_sticker"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "<strong>PNG</strong> image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a <em>file_id</em> as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "tgs_sticker"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "<strong>TGS</strong> animation with the sticker, uploaded using multipart/form-data. See <a href=\"https://core.telegram.org/animated_stickers#technical-requirements\"></a><a href=\"https://core.telegram.org/animated_stickers#technical-requirements\">https://core.telegram.org/animated_stickers#technical-requirements</a> for technical requirements")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "emojis"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "One or more emoji corresponding to the sticker")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "mask_position"
               :TYPE "MaskPosition"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for position where the mask should be placed on faces"))
   :DOC "Use this method to add a new sticker to a set created by the bot. You <strong>must</strong> use exactly one of the fields <em>png_sticker</em> or <em>tgs_sticker</em>. Animated stickers can be added to animated sticker sets and only to them. Animated sticker sets can have up to 50 stickers. Static sticker sets can have up to 120 stickers. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setStickerPositionInSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "sticker"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "File identifier of the sticker")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "position"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "New sticker position in the set, zero-based"))
   :DOC "Use this method to move a sticker in a set created by the bot to a specific position. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "deleteStickerFromSet"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "sticker"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "File identifier of the sticker"))
   :DOC "Use this method to delete a sticker from a set created by the bot. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "setStickerSetThumb"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Sticker set name")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier of the sticker set owner")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "thumb"
               :TYPE "InputFile"
               :REQUIRED "Optional"
               :DESCRIPTION "A <strong>PNG</strong> image with the thumbnail, must be up to 128 kilobytes in size and have width and height exactly 100px, or a <strong>TGS</strong> animation with the thumbnail up to 32 kilobytes in size; see <a href=\"https://core.telegram.org/animated_stickers#technical-requirements\"></a><a href=\"https://core.telegram.org/animated_stickers#technical-requirements\">https://core.telegram.org/animated_stickers#technical-requirements</a> for animated sticker technical requirements. Pass a <em>file_id</em> as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. <a href=\"#sending-files\">More info on Sending Files »</a>. Animated sticker set thumbnail can't be uploaded via HTTP URL."))
   :DOC "Use this method to set the thumbnail of a sticker set. Animated thumbnails can be set for animated sticker sets only. Returns <em>True</em> on success.") 
#S(API-METHOD
   :NAME "answerInlineQuery"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_query_id"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the answered query")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "results"
               :TYPE "InlineQueryResult"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized array of results for the inline query")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "cache_time"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "is_personal"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "next_offset"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don't support pagination. Offset length can't exceed 64 bytes.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "switch_pm_text"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "If passed, clients will display a button with specified text that switches the user to a private chat with the bot and sends the bot a start message with the parameter <em>switch_pm_parameter</em>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "switch_pm_parameter"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "<a href=\"/bots#deep-linking\">Deep-linking</a> parameter for the /start message sent to the bot when user presses the switch button. 1-64 characters, only <code>A-Z</code>, <code>a-z</code>, <code>0-9</code>, <code>_</code> and <code>-</code> are allowed.<br><br><em>Example:</em> An inline bot that sends YouTube videos can ask the user to connect the bot to their YouTube account to adapt search results accordingly. To do this, it displays a 'Connect your YouTube account' button above the results, or even before showing any. The user presses the button, switches to a private chat with the bot and, in doing so, passes a start parameter that instructs the bot to return an oauth link. Once done, the bot can offer a <a href=\"#inlinekeyboardmarkup\"><em>switch_inline</em></a> button so that the user can easily return to the chat where they wanted to use the bot's inline capabilities."))
   :DOC "Use this method to send answers to an inline query. On success, <em>True</em> is returned.<br>No more than <strong>50</strong> results per query are allowed.") 
#S(API-METHOD
   :NAME "sendInvoice"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target private chat")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "title"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Product name, 1-32 characters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "description"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Product description, 1-255 characters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "payload"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "provider_token"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Payments provider token, obtained via <a href=\"https://t.me/botfather\">Botfather</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "start_parameter"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique deep-linking parameter that can be used to generate this invoice when used as a start parameter")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "currency"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Three-letter ISO 4217 currency code, see <a href=\"/bots/payments#supported-currencies\">more on currencies</a>")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "prices"
               :TYPE "LabeledPrice"
               :REQUIRED "Yes"
               :DESCRIPTION "Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "provider_data"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo_url"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service. People like it better when they see what they are paying for.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo_size"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Photo size")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo_width"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Photo width")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "photo_height"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Photo height")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "need_name"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if you require the user's full name to complete the order")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "need_phone_number"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if you require the user's phone number to complete the order")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "need_email"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if you require the user's email address to complete the order")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "need_shipping_address"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if you require the user's shipping address to complete the order")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "send_phone_number_to_provider"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if user's phone number should be sent to provider")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "send_email_to_provider"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if user's email address should be sent to provider")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "is_flexible"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the final price depends on the shipping method")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>. If empty, one 'Pay <code>total price</code>' button will be shown. If not empty, the first button must be a Pay button."))
   :DOC "Use this method to send invoices. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "answerShippingQuery"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "shipping_query_id"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the query to be answered")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "ok"
               :TYPE "Boolean"
               :REQUIRED "Yes"
               :DESCRIPTION "Specify True if delivery to the specified address is possible and False if there are any problems (for example, if delivery to the specified address is not possible)")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "shipping_options"
               :TYPE "ShippingOption"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>ok</em> is True. A JSON-serialized array of available shipping options.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "error_message"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>ok</em> is False. Error message in human readable form that explains why it is impossible to complete the order (e.g. \"Sorry, delivery to your desired address is unavailable'). Telegram will display this message to the user."))
   :DOC "If you sent an invoice requesting a shipping address and the parameter <em>is_flexible</em> was specified, the Bot API will send an <a href=\"#update\">Update</a> with a <em>shipping_query</em> field to the bot. Use this method to reply to shipping queries. On success, True is returned.") 
#S(API-METHOD
   :NAME "answerPreCheckoutQuery"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "pre_checkout_query_id"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the query to be answered")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "ok"
               :TYPE "Boolean"
               :REQUIRED "Yes"
               :DESCRIPTION "Specify <em>True</em> if everything is alright (goods are available, etc.) and the bot is ready to proceed with the order. Use <em>False</em> if there are any problems.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "error_message"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>ok</em> is <em>False</em>. Error message in human readable form that explains the reason for failure to proceed with the checkout (e.g. \"Sorry, somebody just bought the last of our amazing black T-shirts while you were busy filling out your payment details. Please choose a different color or garment!\"). Telegram will display this message to the user."))
   :DOC "Once the user has confirmed their payment and shipping details, the Bot API sends the final confirmation in the form of an <a href=\"#update\">Update</a> with the field <em>pre_checkout_query</em>. Use this method to respond to such pre-checkout queries. On success, True is returned. <strong>Note:</strong> The Bot API must receive an answer within 10 seconds after the pre-checkout query was sent.") 
#S(API-METHOD
   :NAME "setPassportDataErrors"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "errors"
               :TYPE "PassportElementError"
               :REQUIRED "Yes"
               :DESCRIPTION "A JSON-serialized array describing the errors"))
   :DOC "Use this if the data submitted by the user doesn't satisfy the standards your service requires for any reason. For example, if a birthday date seems invalid, a submitted document is blurry, a scan shows evidence of tampering, etc. Supply some details in the error message to make sure the user knows how to correct the issues.") 
#S(API-METHOD
   :NAME "sendGame"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Unique identifier for the target chat")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "game_short_name"
               :TYPE "String"
               :REQUIRED "Yes"
               :DESCRIPTION "Short name of the game, serves as the unique identifier for the game. Set up your games via <a href=\"https://t.me/botfather\">Botfather</a>.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_notification"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Sends the message <a href=\"https://telegram.org/blog/channels-2-0#silent-messages\">silently</a>. Users will receive a notification with no sound.")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_to_message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "If the message is a reply, ID of the original message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "allow_sending_without_reply"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass <em>True</em>, if the message should be sent even if the specified replied-to message is not found")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "reply_markup"
               :TYPE "InlineKeyboardMarkup"
               :REQUIRED "Optional"
               :DESCRIPTION "A JSON-serialized object for an <a href=\"https://core.telegram.org/bots#inline-keyboards-and-on-the-fly-updating\">inline keyboard</a>. If empty, one 'Play game_title' button will be shown. If not empty, the first button must launch the game."))
   :DOC "Use this method to send a game. On success, the sent <a href=\"#message\">Message</a> is returned.") 
#S(API-METHOD
   :NAME "setGameScore"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "User identifier")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "score"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "New score, must be non-negative")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "force"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "disable_edit_message"
               :TYPE "Boolean"
               :REQUIRED "Optional"
               :DESCRIPTION "Pass True, if the game message should not be automatically edited to include the current scoreboard")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the sent message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message"))
   :DOC "Use this method to set the score of the specified user in a game. On success, if the message was sent by the bot, returns the edited <a href=\"#message\">Message</a>, otherwise returns <em>True</em>. Returns an error, if the new score is not greater than the user's current score in the chat and <em>force</em> is <em>False</em>.") 
#S(API-METHOD
   :NAME "getGameHighScores"
   :FIELDS (#S(METHOD-FIELDS-PAIRS
               :PARAMETER "user_id"
               :TYPE "Integer"
               :REQUIRED "Yes"
               :DESCRIPTION "Target user id")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "chat_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Unique identifier for the target chat")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "message_id"
               :TYPE "Integer"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>inline_message_id</em> is not specified. Identifier of the sent message")
            #S(METHOD-FIELDS-PAIRS
               :PARAMETER "inline_message_id"
               :TYPE "String"
               :REQUIRED "Optional"
               :DESCRIPTION "Required if <em>chat_id</em> and <em>message_id</em> are not specified. Identifier of the inline message"))
   :DOC "Use this method to get data for high score tables. Will return the score of the specified user and several of their neighbors in a game. On success, returns an <em>Array</em> of <a href=\"#gamehighscore\">GameHighScore</a> objects.") 
))