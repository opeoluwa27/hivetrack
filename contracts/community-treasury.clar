;; community-treasury
;;
;; This contract implements a secure treasury system for managing community funds.
;; It enables transparent tracking of contributions, expenditures, and allocations
;; to specific projects or tasks. The treasury includes configurable withdrawal rules
;; based on community governance decisions, ensuring funds are managed according to
;; collective priorities while maintaining complete transparency about financial flows.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-UNKNOWN-PROJECT (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-PROJECT-EXISTS (err u104))
(define-constant ERR-PROJECT-LOCKED (err u105))
(define-constant ERR-THRESHOLD-NOT-MET (err u106))
;; Data space definitions
;; Contract admin (initially contract deployer, can be transferred)
(define-data-var contract-admin principal tx-sender)
;; Total treasury balance
(define-data-var treasury-balance uint u0)
;; Map to track individual contribution amounts by principal
(define-map contributors
  principal
  uint
)
;; Map to track project allocations
(define-map projects
  { project-id: (string-ascii 64) }
  {
    description: (string-utf8 256),
    allocated-amount: uint,
    spent-amount: uint,
    locked: bool,
    approval-threshold: uint, ;; Required approvals for withdrawals (if 0, only admin can withdraw)
    created-by: principal,
    created-at: uint,
  }
)
;; Map to track withdrawal approvals
(define-map withdrawal-approvals
  {
    project-id: (string-ascii 64),
    withdrawal-id: uint,
  }
  {
    amount: uint,
    recipient: principal,
    approvers: (list 100 principal),
    executed: bool,
  }
)
;; Withdrawal counter for each project
(define-map project-withdrawal-counters
  { project-id: (string-ascii 64) }
  uint
)
;; Map to track authorized approvers by project
(define-map project-approvers
  {
    project-id: (string-ascii 64),
    approver: principal,
  }
  bool
)
;; Events
(define-data-var last-event-id uint u0)
(define-map events
  { event-id: uint }
  {
    event-type: (string-ascii 20),
    project-id: (optional (string-ascii 64)),
    principal: principal,
    amount: (optional uint),
    data: (optional (string-utf8 256)),
    timestamp: uint,
  }
)
;; Private functions
;; Record an event in the event log
(define-private (record-event
    (event-type (string-ascii 20))
    (project-id (optional (string-ascii 64)))
    (amount (optional uint))
    (data (optional (string-utf8 256)))
  )
  (let ((event-id (+ (var-get last-event-id) u1)))
    (map-set events { event-id: event-id } {
      event-type: event-type,
      project-id: project-id,
      principal: tx-sender,
      amount: amount,
      data: data,
      timestamp: block-height,
    })
    (var-set last-event-id event-id)
    true
  )
)

;; Check if a principal is the contract admin
(define-private (is-contract-admin)
  (is-eq tx-sender (var-get contract-admin))
)

;; Check if a project exists
(define-private (project-exists (project-id (string-ascii 64)))
  (is-some (map-get? projects { project-id: project-id }))
)

;; Get the withdrawals counter for a project, defaulting to 0 if not found
(define-private (get-withdrawal-counter (project-id (string-ascii 64)))
  (default-to u0
    (map-get? project-withdrawal-counters { project-id: project-id })
  )
)

;; Increment the withdrawal counter for a project
(define-private (increment-withdrawal-counter (project-id (string-ascii 64)))
  (let ((current-count (get-withdrawal-counter project-id)))
    (map-set project-withdrawal-counters { project-id: project-id }
      (+ current-count u1)
    )
    (+ current-count u1)
  )
)

;; Check if principal is authorized to approve withdrawals for a project
(define-private (is-project-approver
    (project-id (string-ascii 64))
    (approver principal)
  )
  (default-to false
    (map-get? project-approvers {
      project-id: project-id,
      approver: approver,
    })
  )
)

;; Check if a project is locked
(define-private (is-project-locked (project-id (string-ascii 64)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) false)))
    (get locked project)
  )
)

;; Read-only functions
;; Get treasury total balance
(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

;; Get contributor's total contribution
(define-read-only (get-contribution (contributor principal))
  (default-to u0 (map-get? contributors contributor))
)

;; Get project details
(define-read-only (get-project (project-id (string-ascii 64)))
  (map-get? projects { project-id: project-id })
)

;; Get project's unspent balance
(define-read-only (get-project-balance (project-id (string-ascii 64)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) u0)))
    (- (get allocated-amount project) (get spent-amount project))
  )
)

;; Get withdrawal details
(define-read-only (get-withdrawal
    (project-id (string-ascii 64))
    (withdrawal-id uint)
  )
  (map-get? withdrawal-approvals {
    project-id: project-id,
    withdrawal-id: withdrawal-id,
  })
)

;; Check if principal has approved a specific withdrawal
(define-read-only (has-approved-withdrawal
    (project-id (string-ascii 64))
    (withdrawal-id uint)
    (approver principal)
  )
  (let ((withdrawal (unwrap!
      (map-get? withdrawal-approvals {
        project-id: project-id,
        withdrawal-id: withdrawal-id,
      })
      false
    )))
    (is-some (index-of (get approvers withdrawal) approver))
  )
)

(define-private (event-id-to-key (id uint))
  { event-id: id }
)

(define-private (unwrap-event (key { event-id: uint }))
  (unwrap-panic (map-get? events key))
)

;; Check if a principal is an authorized approver for a project
(define-read-only (is-approver
    (project-id (string-ascii 64))
    (approver principal)
  )
  (is-project-approver project-id approver)
)

;; Public functions
;; Contribute funds to the treasury
(define-public (contribute)
  (let (
      (contribution-amount (stx-get-balance tx-sender))
      (current-contribution (get-contribution tx-sender))
    )
    (asserts! (> contribution-amount u0) ERR-INVALID-AMOUNT)
    ;; Update contributor's total
    (map-set contributors tx-sender (+ current-contribution contribution-amount))
    ;; Update treasury balance
    (var-set treasury-balance (+ (var-get treasury-balance) contribution-amount))
    ;; Record the event
    (record-event "CONTRIBUTION" none (some contribution-amount) none)
    (ok contribution-amount)
  )
)

;; Create a new project allocation
(define-public (create-project
    (project-id (string-ascii 64))
    (description (string-utf8 256))
    (allocated-amount uint)
    (approval-threshold uint)
  )
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (not (project-exists project-id)) ERR-PROJECT-EXISTS)
    (asserts! (<= allocated-amount (var-get treasury-balance))
      ERR-INSUFFICIENT-FUNDS
    )
    ;; Create project record
    (map-set projects { project-id: project-id } {
      description: description,
      allocated-amount: allocated-amount,
      spent-amount: u0,
      locked: false,
      approval-threshold: approval-threshold,
      created-by: tx-sender,
      created-at: block-height,
    })
    ;; Initialize withdrawal counter
    (map-set project-withdrawal-counters { project-id: project-id } u0)
    ;; Add admin as default approver
    (map-set project-approvers {
      project-id: project-id,
      approver: (var-get contract-admin),
    }
      true
    )
    ;; Record event
    (record-event "PROJECT_CREATED" (some project-id) (some allocated-amount)
      (some description)
    )
    (ok true)
  )
)

;; Update project allocation
(define-public (update-project-allocation
    (project-id (string-ascii 64))
    (new-allocation uint)
  )
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (asserts! (not (is-project-locked project-id)) ERR-PROJECT-LOCKED)
    (let (
        (project (unwrap-panic (map-get? projects { project-id: project-id })))
        (current-spent (get spent-amount project))
        (current-allocation (get allocated-amount project))
      )
      ;; Ensure new allocation covers already spent amount
      (asserts! (>= new-allocation current-spent) ERR-INVALID-AMOUNT)
      ;; Ensure treasury can cover additional allocation if increasing
      (if (> new-allocation current-allocation)
        (asserts!
          (<= (- new-allocation current-allocation) (var-get treasury-balance))
          ERR-INSUFFICIENT-FUNDS
        )
        true
      )
      ;; Update project allocation
      (map-set projects { project-id: project-id }
        (merge project { allocated-amount: new-allocation })
      )
      (record-event "ALLOCATION_UPDATED" (some project-id) (some new-allocation)
        none
      )
      (ok true)
    )
  )
)

;; Lock a project to prevent further allocation changes
(define-public (lock-project (project-id (string-ascii 64)))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id }))))
      (map-set projects { project-id: project-id }
        (merge project { locked: true })
      )
      (record-event "PROJECT_LOCKED" (some project-id) none none)
      (ok true)
    )
  )
)

;; Unlock a project
(define-public (unlock-project (project-id (string-ascii 64)))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id }))))
      (map-set projects { project-id: project-id }
        (merge project { locked: false })
      )
      (record-event "PROJECT_UNLOCKED" (some project-id) none none)
      (ok true)
    )
  )
)