
;; title: meeting-minutes
;; version: 1.0.0
;; summary: Public Meeting Minutes Platform
;; description: A government transparency platform for meeting scheduling, 
;;              agenda distribution, public comment collection, and record archiving

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_MEETING_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_COMMENT_TOO_LONG (err u103))
(define-constant ERR_MEETING_FINALIZED (err u104))
(define-constant MAX_COMMENT_LENGTH u1000)

;; data vars
(define-data-var meeting-id-counter uint u0)
(define-data-var comment-id-counter uint u0)

;; data maps
(define-map meetings
  { meeting-id: uint }
  {
    title: (string-ascii 200),
    description: (string-ascii 500),
    scheduled-date: uint,
    status: (string-ascii 20), ;; "scheduled", "in-progress", "completed", "cancelled"
    organizer: principal,
    created-at: uint,
    finalized: bool
  }
)

(define-map meeting-agendas
  { meeting-id: uint }
  { agenda: (string-ascii 2000) }
)

(define-map public-comments
  { comment-id: uint }
  {
    meeting-id: uint,
    commenter: principal,
    comment-text: (string-ascii 1000),
    submitted-at: uint,
    approved: bool
  }
)

(define-map meeting-minutes
  { meeting-id: uint }
  {
    minutes-text: (string-ascii 5000),
    recorded-by: principal,
    recorded-at: uint
  }
)

;; public functions

;; Schedule a new meeting
(define-public (schedule-meeting (title (string-ascii 200)) 
                                (description (string-ascii 500))
                                (scheduled-date uint))
  (let
    ((new-meeting-id (+ (var-get meeting-id-counter) u1)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set meetings
      { meeting-id: new-meeting-id }
      {
        title: title,
        description: description,
        scheduled-date: scheduled-date,
        status: "scheduled",
        organizer: tx-sender,
        created-at: stacks-block-height,
        finalized: false
      }
    )
    (var-set meeting-id-counter new-meeting-id)
    (ok new-meeting-id)
  )
)

;; Set meeting agenda
(define-public (set-agenda (meeting-id uint) (agenda (string-ascii 2000)))
  (let
    ((meeting (unwrap! (map-get? meetings { meeting-id: meeting-id }) ERR_MEETING_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get finalized meeting)) ERR_MEETING_FINALIZED)
    (map-set meeting-agendas { meeting-id: meeting-id } { agenda: agenda })
    (ok true)
  )
)

;; Update meeting status
(define-public (update-meeting-status (meeting-id uint) (new-status (string-ascii 20)))
  (let
    ((meeting (unwrap! (map-get? meetings { meeting-id: meeting-id }) ERR_MEETING_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get finalized meeting)) ERR_MEETING_FINALIZED)
    (map-set meetings
      { meeting-id: meeting-id }
      (merge meeting { status: new-status })
    )
    (ok true)
  )
)

;; Submit public comment
(define-public (submit-comment (meeting-id uint) (comment-text (string-ascii 1000)))
  (let
    ((meeting (unwrap! (map-get? meetings { meeting-id: meeting-id }) ERR_MEETING_NOT_FOUND))
     (new-comment-id (+ (var-get comment-id-counter) u1)))
    (asserts! (<= (len comment-text) MAX_COMMENT_LENGTH) ERR_COMMENT_TOO_LONG)
    (asserts! (not (get finalized meeting)) ERR_MEETING_FINALIZED)
    (map-set public-comments
      { comment-id: new-comment-id }
      {
        meeting-id: meeting-id,
        commenter: tx-sender,
        comment-text: comment-text,
        submitted-at: stacks-block-height,
        approved: false
      }
    )
    (var-set comment-id-counter new-comment-id)
    (ok new-comment-id)
  )
)

;; Approve public comment
(define-public (approve-comment (comment-id uint))
  (let
    ((comment (unwrap! (map-get? public-comments { comment-id: comment-id }) ERR_MEETING_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set public-comments
      { comment-id: comment-id }
      (merge comment { approved: true })
    )
    (ok true)
  )
)

;; Record meeting minutes
(define-public (record-minutes (meeting-id uint) (minutes-text (string-ascii 5000)))
  (let
    ((meeting (unwrap! (map-get? meetings { meeting-id: meeting-id }) ERR_MEETING_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get finalized meeting)) ERR_MEETING_FINALIZED)
    (map-set meeting-minutes
      { meeting-id: meeting-id }
      {
        minutes-text: minutes-text,
        recorded-by: tx-sender,
        recorded-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Finalize meeting record
(define-public (finalize-meeting (meeting-id uint))
  (let
    ((meeting (unwrap! (map-get? meetings { meeting-id: meeting-id }) ERR_MEETING_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get finalized meeting)) ERR_MEETING_FINALIZED)
    (map-set meetings
      { meeting-id: meeting-id }
      (merge meeting { finalized: true, status: "completed" })
    )
    (ok true)
  )
)

;; read only functions

;; Get meeting details
(define-read-only (get-meeting (meeting-id uint))
  (map-get? meetings { meeting-id: meeting-id })
)

;; Get meeting agenda
(define-read-only (get-agenda (meeting-id uint))
  (map-get? meeting-agendas { meeting-id: meeting-id })
)

;; Get meeting minutes
(define-read-only (get-minutes (meeting-id uint))
  (map-get? meeting-minutes { meeting-id: meeting-id })
)

;; Get comment details
(define-read-only (get-comment (comment-id uint))
  (map-get? public-comments { comment-id: comment-id })
)

;; Get current meeting counter
(define-read-only (get-meeting-count)
  (var-get meeting-id-counter)
)

;; Get current comment counter
(define-read-only (get-comment-count)
  (var-get comment-id-counter)
)

;; Check if user is owner
(define-read-only (is-owner (user principal))
  (is-eq user CONTRACT_OWNER)
)
