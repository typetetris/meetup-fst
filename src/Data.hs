module Meetup.Data where

import Data.Aeson

data PhotoType = EventPhoto | MemberPhoto deriving (Eq, Show, Generic)

photoTypeToJSON :: PhotoType -> Text
photoTypeToJSON EventPhoto  = "event"
photoTypeToJSON MemberPhoto = "member"

instance ToJSON PhotoType where
  toJSON = genericToJSONString photoTypeToJSON
  toEncoding = genericToEncodingString photoTypeToJSON
instance FromJSON PhotoType where parseJSON = genericParseJSONString photoTypeToJSON

data Photo = Photo { photo_base_url     :: Text
                   , photo_highres_link :: Text
                   , photo_id           :: Integer
                   , photo_photo_link   :: Text
                   , photo_thumb_link   :: Text
                   , photo_type         :: PhotoType
                   } deriving (Eq, Show, Generic)

myStripPrefix :: String -> String -> String
myStripPrefix p x = fromMaybe (error ("du hansel hascht des " <> p <> " vergessen!")) (L.stripPrefix p x)

myOptions :: String -> Options
myOptions prefix = defaultOptions { fieldLabelModifier = myStripPrefix prefix }

instance ToJSON Photo where
  toJSON = genericToJSON (myOptions "photo_")
  toEncoding = genericToEncoding (myOptions "photo_")
instance FromJSON Photo where parseJSON = genericParseJSON (myOptions "photo_")

data Member = Member { member_id    :: Integer
                     , member_intro :: Text
                     , member_name  :: Text
                     , member_photo :: Photo
                     } deriving (Eq, Show, Generic)

instance ToJSON Member where
  toJSON = genericToJSON (myOptions "member_")
  toEncoding = genericToEncoding (myOptions "member_")
instance FromJSON Member where parseJSON = genericParseJSON (myOptions "member_")

data PaymentMethod = Paypal | Amazon | Wepay | Cash deriving (Eq, Show, Generic)

paymentMethodToJSON :: PaymentMethod -> Text
paymentMethodToJSON Paypal = "paypal"
paymentMethodToJSON Amazon = "amazon"
paymentMethodToJSON Wepay  = "wepay"
paymentMethodToJSON Cash   = "cash"

instance ToJSON PaymentMethod where
  toJSON = genericToJSONString paymentMethodToJSON
  toEncoding = genericToEncodingString paymentMethodToJSON

instance FromJSON PaymentMethod where
  parseJSON = genericParseJSONString paymentMethodToJSON

data FeeInfo = FeeInfo { feeInfo_accepts     :: PaymentMethod
                       , feeInfo_amount      :: Text
                       , feeInfo_currency    :: Text
                       , feeInfo_description :: Text
                       , feeInfo_label       :: Text
                       , feeInfo_required    :: Bool
                       } deriving (Eq, Show, Generic)

instance ToJSON FeeInfo where
  toJSON = genericToJSON (myOptions "feeInfo_")
  toEncoding = genericToEncoding (myOptions "feeInfo_")
instance FromJSON FeeInfo where parseJSON = genericParseJSON (myOptions "feeInfo_")

data Category = Category { category_best_topics :: Maybe Text -- only included if requested in fields as best_topics
                         , category_ids         :: Maybe [Integer]
                         , category_id          :: Integer
                         , category_name        :: Text
                         , category_photo       :: Maybe Photo
                         , category_shortname   :: Text
                         , category_sort_name   :: Text
                         } deriving (Eq, Show, Generic)

instance ToJSON Category where
  toJSON = genericToJSON (myOptions "category_")
  toEncoding = genericToEncoding (myOptions "category_")
instance FromJSON Category where parseJSON = genericParseJSON (myOptions "category_")

data GroupJoinInfo = GroupJoinInfo { groupJoinInfo_photo_req     :: Bool
                                   , groupJoinInfo_questions     :: [Text]
                                   , groupJoinInfo_questions_req :: Bool
                                   } deriving (Eq, Show, Generic)

instance ToJSON GroupJoinInfo where
  toJSON = genericToJSON (myOptions "groupJoinInfo_")
  toEncoding = genericToEncoding (myOptions "groupJoinInfo_")
instance FromJSON GroupJoinInfo where parseJSON = genericParseJSON (myOptions "groupJoinInfo_")

data FeeReason = CompensateOrganizer |
                 CoverCosts |
                 EncourageEngagement |
                 ImproveMeetups |
                 Other |
                 ProvideSupplies |
                 ReserveFund deriving (Eq, Show, Generic)

feeReasonToJSON :: FeeReason -> Text
feeReasonToJSON CompensateOrganizer = "compensate_organizer"
feeReasonToJSON CoverCosts          = "cover_costs"
feeReasonToJSON EncourageEngagement = "encourage_engagement"
feeReasonToJSON ImproveMeetups      = "improve_meetups"
feeReasonToJSON Other               = "other"
feeReasonToJSON ProvideSupplies     = "provide_supplies"
feeReasonToJSON ReserveFund         = "reserve_fund"

instance ToJSON FeeReason where
  toJSON = genericToJSONString feeReasonToJSON
  toEncoding = genericToEncodingString feeReasonToJSON

instance FromJSON FeeReason where
  parseJSON = genericParseJSONString feeReasonToJSON

data MembershipDues = MembershipDues { membershipDues_currency :: Text
                                     , membershipDues_fee :: Scientific
                                     , membershipDues_fee_desc :: Text
                                     , membershipDues_methods :: Text
                                     , membershipDues_reasons :: [FeeReason]
                                     , membershipDues_reasons_other :: Maybe Text
                                     , membershipDues_refund_policy :: Text
                                     , membershipDues_required :: Bool
                                     , membershipDues_requered_to :: FeeRequiredTo
                                     , membershipDues_self_payment_required :: Bool
                                     , membershipDues_trial_days :: Maybe Int
                                     } deriving (Eq, Show, Generic)

instance ToJSON MembershipDues where
  toJSON = genericToJSON (myOptions "membershipDues_")
  toEncoding = genericToEncoding (myOptions "membershipDues_")
instance FromJSON MembershipDues where parseJSON = genericParseJSON (myOptions "membershipDues_")

data FeeRequiredTo = Join deriving (Eq, Show, Generic)

feeRequiredToToJSON :: FeeRequiredTo -> Text
feeRequiredToToJSON Join = "join"

instance ToJSON FeeRequiredTo where
  toJSON = genericToJSONString feeRequiredToToJSON
  toEncoding = genericToEncodingString feeRequiredToToJSON

instance FromJSON FeeRequiredTo where
  parseJSON = genericParseJSONString feeRequiredToToJSON

data Group = Group { group_category           :: Maybe Category -- only included if requested in fields as group_category
                   , group_id                 :: Integer
                   , group_join_info          :: Maybe GroupJoinInfo -- only included if requested in fields as group_join_info
                   , group_join_mode          :: GroupJoinMode
                   , group_key_photo          :: Maybe Photo -- only included if requested in fields as group_key_photo
                   , group_lat                :: Scientific
                   , group_localized_location :: Text
                   , group_lon                :: Scientific
                   , group_membership_dues    :: Maybe MembershipDues
                   , group_meta_category      :: Maybe Category -- only included if requested in fields as meta_category
                   , group_name               :: Text
                   , group_past_event_count   :: Maybe Integer -- requested by group_past_event_count
                   , group_photo              :: Maybe Photo -- requested by group_photo
                   , group_photo_gradient     :: Maybe PhotoGradient -- requested by group_photo_gradient
                   , group_region             :: Text
                   , group_self               :: Maybe Self -- requested by one of group_self_actions, group_self_profile, group_self_membership_dues or group_self_status
                   , group_topics             :: Maybe [Topic]
                   , group_urlname            :: Text
                   , group_visibility         :: Maybe Visibility -- requested by group_visibility
                   , group_who                :: Text
                   } deriving (Eq, Show, Generic)

instance ToJSON Group where
  toJSON = genericToJSON (myOptions "group_")
  toEncoding = genericToEncoding (myOptions "group_")
instance FromJSON Group where parseJSON = genericParseJSON (myOptions "group_")

data Visibility = Public | PublicLimited | Members deriving (Eq, Show, Generic)

visibilityToJSON :: Visibility -> Text
visibilityToJSON Public        = "public"
visibilityToJSON PublicLimited = "public_limited"
visibilityToJSON Members       = "members"

instance ToJSON Visibility where
  toJSON = genericToJSONString visibilityToJSON
  toEncoding = genericToEncodingString visibilityToJSON

instance FromJSON Visibility where
  parseJSON = genericParseJSONString visibilityToJSON

data VenueVisibility = VVPublic | VVMembers deriving (Eq, Show, Generic)

venueVisibilityToJSON :: VenueVisibility -> Text
venueVisibilityToJSON VVPublic        = "public"
venueVisibilityToJSON VVMembers       = "members"

instance ToJSON VenueVisibility where
  toJSON = genericToJSONString venueVisibilityToJSON
  toEncoding = genericToEncodingString venueVisibilityToJSON

instance FromJSON VenueVisibility where
  parseJSON = genericParseJSONString venueVisibilityToJSON

data Topic = Topic { topic_id     :: Integer
                   , topic_lang   :: Text
                   , topic_name   :: Text
                   , topic_urlkey :: Text
                   } deriving (Eq, Show, Generic)

instance ToJSON Topic where
  toJSON = genericToJSON (myOptions "topic_")
  toEncoding = genericToEncoding (myOptions "topic_")
instance FromJSON Topic where parseJSON = genericParseJSON (myOptions "topic_")


data Self = Self { self_actions         :: Actions
                 , self_membership_dues :: Maybe MembershipDues -- requested by group_membership_dues
                 , self_profile         :: Text
                 , self_status          :: SelfStatus
                 } deriving (Eq, Show, Generic)

instance ToJSON Self where
  toJSON = genericToJSON (myOptions "self_")
  toEncoding = genericToEncoding (myOptions "self_")
instance FromJSON Self where parseJSON = genericParseJSON (myOptions "self_")

data Actions = EventCreate |
               EventDraft |
               RoleAssign |
               Edit |
               MemberApproval |
               SubscriptionsUpgrade deriving (Eq, Show, Generic)

actionsToJSON :: Actions -> Text
actionsToJSON EventCreate          = "event_create"
actionsToJSON EventDraft           = "event_draft"
actionsToJSON RoleAssign           = "role_assign"
actionsToJSON Edit                 = "edit"
actionsToJSON MemberApproval       = "member_approval"
actionsToJSON SubscriptionsUpgrade = "subscriptions_upgrade"

instance ToJSON Actions where
  toJSON = genericToJSONString actionsToJSON
  toEncoding = genericToEncodingString actionsToJSON

instance FromJSON Actions where
  parseJSON = genericParseJSONString actionsToJSON

data SelfStatus = None |
                  Pending |
                  PendingPayment |
                  Active |
                  Blocked deriving (Eq, Show, Generic)

selfStatusToJSON :: SelfStatus -> Text
selfStatusToJSON None           = "none"
selfStatusToJSON Pending        = "pending"
selfStatusToJSON PendingPayment = "pending_payment"
selfStatusToJSON Active         = "active"
selfStatusToJSON Blocked        = "blocked"

instance ToJSON SelfStatus where
  toJSON = genericToJSONString selfStatusToJSON
  toEncoding = genericToEncodingString selfStatusToJSON

instance FromJSON SelfStatus where
  parseJSON = genericParseJSONString selfStatusToJSON

data PhotoGradient = PhotoGradient { photoGradient_composite_color :: Text
                                   , photoGradient_dark_color      :: Text
                                   , photoGradient_id              :: Integer
                                   , photoGradient_light_color     :: Text
                                   } deriving (Eq, Show, Generic)

instance ToJSON PhotoGradient where
  toJSON = genericToJSON (myOptions "photoGradient_")
  toEncoding = genericToEncoding (myOptions "photoGradient_")
instance FromJSON PhotoGradient where parseJSON = genericParseJSON (myOptions "photoGradient_")

data GroupJoinMode = Open | Approval | Closed deriving (Eq, Show, Generic)

groupJoinModeToJSON :: GroupJoinMode -> Text
groupJoinModeToJSON Open     = "open"
groupJoinModeToJSON Approval = "approval"
groupJoinModeToJSON Closed   = "closed"

instance ToJSON GroupJoinMode where
  toJSON = genericToJSONString groupJoinModeToJSON
  toEncoding = genericToEncodingString groupJoinModeToJSON

instance FromJSON GroupJoinMode where
  parseJSON = genericParseJSONString groupJoinModeToJSON


data Event = Event { event_comment_count                    :: Maybe Int
                   , event_created                          :: Integer
                   , event_description                      :: Text
                   , event_description_images               :: Maybe [Text]
                   , event_duration                         :: Maybe Int
                   , event_event_hosts                      :: Maybe [Member] -- only included if requested in fields
                   , event_featured                         :: Maybe Bool -- only included if requested in fields
                   , event_featured_photo                   :: Maybe Photo -- only included if requested in fields
                   , event_fee                              :: Maybe FeeInfo -- not present in tests, so maybe only it there is a fee.
                   , event_group                            :: Group
                   , event_how_to_find_us                   :: Maybe Text -- requested by how_to_find_us
                   , event_id                               :: Text
                   , event_link                             :: Text
                   , event_local_date                       :: Day
                   , event_local_time                       :: TimeOfDay
                   , event_manual_attendance_count          :: Maybe Integer
                   , event_name                             :: Text
                   , event_photo_album                      :: Maybe PhotoAlbum
                   , event_plain_text_description           :: Maybe Text -- requested by plain_text_description
                   , event_plain_text_no_images_description :: Maybe Text -- requested by plain_text_no_images_description
                   , event_rsvp_close_offset                :: Maybe DiffTime -- not present in test, maybe only if set
                   , event_rsvp_limit                       :: Maybe Integer -- not present in test, maybe only if set
                   , event_rsvp_open_offset                 :: Maybe DiffTime -- not present in test, maybe only if set
                   , event_rsvp_rules                       :: Maybe RSVPRules -- requested by rsvp_rules
                   , event_rsvp_sample                      :: Maybe [RSVP] -- requested by rsvp_sample
                   , event_rsvpable                  :: Maybe Bool -- requested by rsvpable
                   , event_rsvpable_after_join :: Maybe Bool -- requested by rsvpable_after_join
                   , event_saved :: Maybe Bool -- requested by saved
                   , event_self :: Maybe EventSelf -- requested by self
                   , event_series :: Maybe EventSeries -- requested by series
                   , event_short_link :: Maybe Text -- requested by short_link
                   , event_simple_html_description :: Maybe Text -- requested by simple_html_description
                   , event_status :: Status
                   , event_survey_questions :: Maybe [Question] -- requested by answers
                   , event_time :: Integer
                   , event_updated :: Integer
                   , event_utc_offset :: Integer
                   , event_venue :: Venue
                   , event_venue_visibility :: Maybe VenueVisibility -- requested by venue_visibility
                   , event_visibility :: Visibility
                   , event_waitlist_count :: Maybe Integer
                   , event_why :: Maybe Text -- n.p.i.t.
                   , event_yes_rsvp_count :: Integer
                   } deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON = genericToJSON (myOptions "event_")
  toEncoding = genericToEncoding (myOptions "event_")
instance FromJSON Event where parseJSON = genericParseJSON (myOptions "event_")

data Venue = Venue { venue_address_1 :: Text
                   , venue_address_2 :: Maybe Text -- not present in test, maybe only if set
                   , venue_address_3 :: Maybe Text -- not present in test, maybe only if set
                   , venue_city :: Text
                   , venue_country :: Text
                   , venue_id :: Integer
                   , venue_lat :: Scientific
                   , venue_localized_country_name :: Text
                   , venue_lon :: Scientific
                   , venue_name :: Text
                   , venue_phone :: Maybe Text -- not present in test, maybe only if set
                   , venue_repinned :: Bool
                   , venue_state :: Maybe Text -- not present in test, maybe only if set
                   , venue_zip :: Maybe Text -- n.p.i.t.
                   } deriving (Eq, Show, Generic)

instance ToJSON Venue where
  toJSON = genericToJSON (myOptions "venue_")
  toEncoding = genericToEncoding (myOptions "venue_")
instance FromJSON Venue where parseJSON = genericParseJSON (myOptions "venue_")


data Question = Question { question_id :: Integer
                         , question_question :: Text
                         } deriving (Eq, Show, Generic)

instance ToJSON Question where
  toJSON = genericToJSON (myOptions "question_")
  toEncoding = genericToEncoding (myOptions "question_")
instance FromJSON Question where parseJSON = genericParseJSON (myOptions "question_")


data EventSeries = EventSeries { eventSeries_description :: Text
                               , eventSeries_end_date :: Integer
                               , eventSeries_id :: Integer
                               , eventSeries_monthly :: Maybe Monthly
                               , eventSeries_start_date :: Integer
                               , eventSeries_template_event_id :: Integer
                               , eventSeries_weekly :: Maybe Weekly
                               } deriving (Eq, Show, Generic)
instance ToJSON EventSeries where
  toJSON = genericToJSON (myOptions "eventSeries_")
  toEncoding = genericToEncoding (myOptions "eventSeries_")
instance FromJSON EventSeries where parseJSON = genericParseJSON (myOptions "eventSeries_")

data Monthly = Monthly { monthly_day_of_week :: Int
                       , monthly_interval :: Integer
                       , monthly_week_of_month :: Int
                       } deriving (Eq, Show, Generic)

instance ToJSON Monthly where
  toJSON = genericToJSON (myOptions "monthly_")
  toEncoding = genericToEncoding (myOptions "monthly_")
instance FromJSON Monthly where parseJSON = genericParseJSON (myOptions "monthly_")

data Weekly = Weekly { weekly_days_of_week :: [Int]
                     , weekly_interval :: Integer
                     } deriving (Eq, Show, Generic)

instance ToJSON Weekly where
  toJSON = genericToJSON (myOptions "weekly_")
  toEncoding = genericToEncoding (myOptions "weekly_")
instance FromJSON Weekly where parseJSON = genericParseJSON (myOptions "weekly_")



data EventSelf = EventSelf { eventSelf_actions :: EventSelfActions
                           , eventSelf_pay_status :: EventSelfPayStatus
                           , eventSelf_role :: Role
                           , eventSelf_rsvp :: EventSelfRSVP
                           } deriving (Eq, Show, Generic)

instance ToJSON EventSelf where
  toJSON = genericToJSON (myOptions "eventSelf_")
  toEncoding = genericToEncoding (myOptions "eventSelf_")
instance FromJSON EventSelf where parseJSON = genericParseJSON (myOptions "eventSelf_")

data EventSelfRSVP = EventSelfRSVP { eventSelfRSVP_answers :: [Text]
                                   , eventSelfRSVP_guests :: Integer
                                   , eventSelfRSVP_response :: Response
                                   } deriving (Eq, Show, Generic)

instance ToJSON EventSelfRSVP where
  toJSON = genericToJSON (myOptions "eventSelfRSVP_")
  toEncoding = genericToEncoding (myOptions "eventSelfRSVP_")
instance FromJSON EventSelfRSVP where parseJSON = genericParseJSON (myOptions "eventSelfRSVP_")

data Response = Yes | No | Waitlist | YesPendingPayment deriving (Eq, Show, Generic)

responseToJSON :: Response -> Text
responseToJSON Yes = "yes"
responseToJSON No = "no"
responseToJSON Waitlist = "waitlist"
responseToJSON YesPendingPayment = "yes_pending_payment"

instance ToJSON Response where
  toJSON = genericToJSONString responseToJSON
  toEncoding = genericToEncodingString responseToJSON

instance FromJSON Response where
  parseJSON = genericParseJSONString responseToJSON

data EventSelfActions = Announce |
                        Attendance |
                        Copy |
                        Comment |
                        Payments |
                        Publish |
                        EventSelfActionsEdit |
                        EditHosts |
                        EmailAttendees |
                        Delete |
                        Rsvp |
                        Wait |
                        Dues |
                        UploadPhoto
        deriving (Eq, Show, Generic)

eventSelfActionsToJSON :: EventSelfActions -> Text
eventSelfActionsToJSON Announce = "announce"
eventSelfActionsToJSON Attendance = "attendance"
eventSelfActionsToJSON Copy = "copy"
eventSelfActionsToJSON Comment = "comment"
eventSelfActionsToJSON Payments = "payments"
eventSelfActionsToJSON Publish = "publish"
eventSelfActionsToJSON EventSelfActionsEdit = "edit"
eventSelfActionsToJSON EditHosts = "edit_hosts"
eventSelfActionsToJSON EmailAttendees = "email_attendees"
eventSelfActionsToJSON Delete = "delete"
eventSelfActionsToJSON Rsvp = "rsvp"
eventSelfActionsToJSON Wait = "wait"
eventSelfActionsToJSON Dues = "dues"
eventSelfActionsToJSON UploadPhoto = "upload_photo"

instance ToJSON EventSelfActions where
  toJSON = genericToJSONString eventSelfActionsToJSON
  toEncoding = genericToEncodingString eventSelfActionsToJSON

instance FromJSON EventSelfActions where
  parseJSON = genericParseJSONString eventSelfActionsToJSON

data EventSelfPayStatus = ESPSNone |
                          Paid |
                          PartiallyPaid |
                          PaymentPending |
                          EcheckPending |
                          RefundPending |
                          PartiallyRefunded |
                          Refunded
                          deriving (Eq, Show, Generic)

eventSelfPayStatusToJSON ESPSNone = "none"
eventSelfPayStatusToJSON Paid = "paid"
eventSelfPayStatusToJSON PartiallyPaid = "partially_paid"
eventSelfPayStatusToJSON PaymentPending = "payment_pending"
eventSelfPayStatusToJSON EcheckPending = "echeck_pending"
eventSelfPayStatusToJSON RefundPending = "refund_pending"
eventSelfPayStatusToJSON PartiallyRefunded = "Partially_refunded"
eventSelfPayStatusToJSON Refunded = "refunded"

instance ToJSON EventSelfPayStatus where
  toJSON = genericToJSONString eventSelfPayStatusToJSON
  toEncoding = genericToEncodingString eventSelfPayStatusToJSON

instance FromJSON EventSelfPayStatus where
  parseJSON = genericParseJSONString eventSelfPayStatusToJSON

data RSVP = RSVP { rsvp_created :: Integer
                 , rsvp_id :: Integer
                 , rsvp_member :: RSVPMember
                 , rsvp_updated :: Integer
                 } deriving (Eq, Show, Generic)

instance ToJSON RSVP where
  toJSON = genericToJSON (myOptions "rsvp_")
  toEncoding = genericToEncoding (myOptions "rsvp_")
instance FromJSON RSVP where parseJSON = genericParseJSON (myOptions "rsvp_")

data RSVPMember = RSVPMember { rsvpMember_bio :: Text
                             , rsvpMember_event_context :: Maybe Text
                             , rsvpMember_id :: Integer
                             , rsvpMember_name :: Text
                             , rsvpMember_photo :: Maybe Photo
                             , rsvpMember_role :: Role
                             , rsvpMember_self :: Maybe Text -- requested by self
                             , rsvpMember_title :: Maybe Text
                             } deriving (Eq, Show, Generic)

instance ToJSON RSVPMember where
  toJSON = genericToJSON (myOptions "rsvpMember_")
  toEncoding = genericToEncoding (myOptions "rsvpMember_")
instance FromJSON RSVPMember where parseJSON = genericParseJSON (myOptions "rsvpMember_")

data Role = AssistantOrganizer |
            CoOrganizer |
            EventOrganizer |
            Organizer deriving (Eq, Show, Generic)

roleToJSON :: Role -> Text
roleToJSON AssistantOrganizer = "assistant_organizer"
roleToJSON CoOrganizer = "coorganizer"
roleToJSON EventOrganizer = "event_organizer"
roleToJSON Organizer = "organizer"

instance ToJSON Role where
  toJSON = genericToJSONString roleToJSON
  toEncoding = genericToEncodingString roleToJSON

instance FromJSON Role where
  parseJSON = genericParseJSONString roleToJSON


data RSVPRules = RSVPRules { rsvpRules_close_time    :: UTCTime
                           , rsvpRules_closed        :: Bool
                           , rsvpRules_guest_limit   :: Integer
                           , rsvpRules_open_time     :: UTCTime
                           , rsvpRules_refund_policy :: RefundPolicy
                           , rsvpRules_waitlisting   :: Waitlisting
                           } deriving (Eq, Show, Generic)

instance ToJSON RSVPRules where
  toJSON = genericToJSON (myOptions "rsvpRules_")
  toEncoding = genericToEncoding (myOptions "rsvpRules_")
instance FromJSON RSVPRules where parseJSON = genericParseJSON (myOptions "rsvpRules_")

data Waitlisting = Auto | Manual | Off deriving (Eq, Show, Generic)

waitlistingToJSON :: Waitlisting -> Text
waitlistingToJSON Auto   = "auto"
waitlistingToJSON Manual = "manual"
waitlistingToJSON Off    = "off"

instance ToJSON Waitlisting where
  toJSON = genericToJSONString waitlistingToJSON
  toEncoding = genericToEncodingString waitlistingToJSON

instance FromJSON Waitlisting where
  parseJSON = genericParseJSONString waitlistingToJSON

data RefundPolicy = RefundPolicy { refundPolicy_days     :: Maybe Integer
                                 , refundPolicy_notes    :: Text
                                 , refundPolicy_policies :: RefundPolicies
                                 } deriving (Eq, Show, Generic)

instance ToJSON RefundPolicy where
  toJSON = genericToJSON (myOptions "refundPolicy_")
  toEncoding = genericToEncoding (myOptions "refundPolicy_")
instance FromJSON RefundPolicy where parseJSON = genericParseJSON (myOptions "refundPolicy_")

data RefundPolicies = NoRefunds |
                      MemberCancellation |
                      EventCancellation |
                      EventRescheduled deriving (Eq, Show, Generic)

refundPoliciesToJSON NoRefunds          = "no_refunds"
refundPoliciesToJSON MemberCancellation = "member_cancellation"
refundPoliciesToJSON EventCancellation  = "event_cancellation"
refundPoliciesToJSON EventRescheduled   = "event_rescheduled"

instance ToJSON RefundPolicies where
  toJSON = genericToJSONString refundPoliciesToJSON
  toEncoding = genericToEncodingString refundPoliciesToJSON

instance FromJSON RefundPolicies where
  parseJSON = genericParseJSONString refundPoliciesToJSON

data PhotoAlbum = PhotoAlbum { photoAlbum_event        :: Maybe PhotoAlbumEvent
                             , photoAlbum_id           :: Integer
                             , photoAlbum_photo_count  :: Integer
                             , photoAlbum_photo_sample :: [Photo]
                             , photoAlbum_title        :: Text
                             } deriving (Eq, Show, Generic)

instance ToJSON PhotoAlbum where
  toJSON = genericToJSON (myOptions "photoAlbum_")
  toEncoding = genericToEncoding (myOptions "photoAlbum_")
instance FromJSON PhotoAlbum where parseJSON = genericParseJSON (myOptions "photoAlbum_")

data PhotoAlbumEvent = PhotoAlbumEvent { photoAlbumEvent_id :: Text
                                       , photoAlbumEvent_name :: Text
                                       , photoAlbumEvent_no_rsvp_count :: Integer -- requested by rsvp_counts
                                       , photoAlbumEvent_time :: Integer
                                       , photoAlbumEvent_utc_offset :: Integer
                                       , photoAlbumEvent_waitlist_count :: Integer -- requested by rsvp_counts
                                       , photoAlbumEvent_yes_rsvp_count :: Integer
                                       } deriving (Eq, Show, Generic)

instance ToJSON PhotoAlbumEvent where
  toJSON = genericToJSON (myOptions "photoAlbumEvent_")
  toEncoding = genericToEncoding (myOptions "photoAlbumEvent_")
instance FromJSON PhotoAlbumEvent where parseJSON = genericParseJSON (myOptions "photoAlbumEvent_")

data Scroll = RecentPast |
              NextUpcoming |
              FutureOrPast deriving (Eq, Show, Generic)

scrollToText :: Scroll -> Text
scrollToText RecentPast = "recent_past"
scrollToText NextUpcoming = "next_upcoming"
scrollToText FutureOrPast = "future_or_past"

instance ToHttpApiData Scroll where toUrlPiece = scrollToText

data Status = Cancelled | Draft | Past | Proposed | Suggested | Upcoming deriving (Eq, Show, Generic)

statusToJSON :: Status -> Text
statusToJSON Cancelled = "cancelled"
statusToJSON Draft = "draft"
statusToJSON Past = "past"
statusToJSON Proposed = "proposed"
statusToJSON Suggested = "suggested"
statusToJSON Upcoming = "upcoming"

instance ToJSON Status where
  toJSON = genericToJSONString statusToJSON
  toEncoding = genericToEncodingString statusToJSON

instance FromJSON Status where
  parseJSON = genericParseJSONString statusToJSON

instance ToHttpApiData Status where toUrlPiece = statusToJSON
