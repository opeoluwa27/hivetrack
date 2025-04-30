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
(define-map contributors principal uint)

;; Map to track project allocations
(define-map projects 
  { project-id: (string-ascii 64) }
  {
    description: (string-utf8 256),
    allocated-amount: uint,
    spent-amount: uint,
    locked: bool,
    approval-threshold: uint,    ;; Required approvals for withdrawals (if 0, only admin can withdraw)
    created-by: principal,
    created-at: uint
  }
)

;; Map to track withdrawal approvals
(define-map withdrawal-approvals
  { project-id: (string-ascii 64), withdrawal-id: uint }
  {
    amount: uint, 
    recipient: principal,
    approvers: (list 100 principal),
    executed: bool
  }
)

;; Withdrawal counter for each project
(define-map project-withdrawal-counters { project-id: (string-ascii 64) } uint)

;; Map to track authorized approvers by project
(define-map project-approvers { project-id: (string-ascii 64), approver: principal } bool)

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
    timestamp: uint
  }
)

;; Private functions

;; Record an event in the event log
(define-private (record-event (event-type (string-ascii 20)) 
                             (project-id (optional (string-ascii 64))) 
                             (amount (optional uint)) 
                             (data (optional (string-utf8 256))))
  (let ((event-id (+ (var-get last-event-id) u1)))
    (map-set events
      { event-id: event-id }
      {
        event-type: event-type,
        project-id: project-id,
        principal: tx-sender,
        amount: amount,
        data: data,
        timestamp: block-height
      }
    )
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
  (default-to u0 (map-get? project-withdrawal-counters { project-id: project-id }))
)

;; Increment the withdrawal counter for a project
(define-private (increment-withdrawal-counter (project-id (string-ascii 64)))
  (let ((current-count (get-withdrawal-counter project-id)))
    (map-set project-withdrawal-counters { project-id: project-id } (+ current-count u1))
    (+ current-count u1)
  )
)

;; Check if principal is authorized to approve withdrawals for a project
(define-private (is-project-approver (project-id (string-ascii 64)) (approver principal))
  (default-to false (map-get? project-approvers { project-id: project-id, approver: approver }))
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
(define-read-only (get-withdrawal (project-id (string-ascii 64)) (withdrawal-id uint))
  (map-get? withdrawal-approvals { project-id: project-id, withdrawal-id: withdrawal-id })
)

;; Check if principal has approved a specific withdrawal
(define-read-only (has-approved-withdrawal (project-id (string-ascii 64)) (withdrawal-id uint) (approver principal))
  (let ((withdrawal (unwrap! (map-get? withdrawal-approvals { project-id: project-id, withdrawal-id: withdrawal-id }) false)))
    (is-some (index-of (get approvers withdrawal) approver))
  )
)

;; Get events within a range
(define-read-only (get-events (start-id uint) (end-id uint))
  (let ((max-id (var-get last-event-id)))
    (if (> end-id max-id)
      (list)
      (map unwrap-event (map-id-range start-id end-id))
    )
  )
)

;; Helper for mapping event ID to event data
(define-private (map-id-range (start-id uint) (end-id uint))
  (map event-id-to-key (unwrap-panic (slice? (list start-id) (- (+ end-id u1) start-id))))
)

(define-private (event-id-to-key (id uint))
  { event-id: id }
)

(define-private (unwrap-event (key { event-id: uint }))
  (unwrap-panic (map-get? events key))
)

;; Check if a principal is an authorized approver for a project
(define-read-only (is-approver (project-id (string-ascii 64)) (approver principal))
  (is-project-approver project-id approver)
)

;; Public functions

;; Transfer admin rights to a new principal
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (record-event "ADMIN_TRANSFER" none none (some (concat "New admin: " (principal->string new-admin))))
    (ok true)
  )
)

;; Contribute funds to the treasury
(define-public (contribute)
  (let ((contribution-amount (stx-get-balance tx-sender))
        (current-contribution (get-contribution tx-sender)))
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
(define-public (create-project (project-id (string-ascii 64)) 
                              (description (string-utf8 256)) 
                              (allocated-amount uint)
                              (approval-threshold uint))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (not (project-exists project-id)) ERR-PROJECT-EXISTS)
    (asserts! (<= allocated-amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create project record
    (map-set projects
      { project-id: project-id }
      {
        description: description,
        allocated-amount: allocated-amount,
        spent-amount: u0,
        locked: false,
        approval-threshold: approval-threshold,
        created-by: tx-sender,
        created-at: block-height
      }
    )
    
    ;; Initialize withdrawal counter
    (map-set project-withdrawal-counters { project-id: project-id } u0)
    
    ;; Add admin as default approver
    (map-set project-approvers { project-id: project-id, approver: (var-get contract-admin) } true)
    
    ;; Record event
    (record-event "PROJECT_CREATED" (some project-id) (some allocated-amount) (some description))
    
    (ok true)
  )
)

;; Add an approver for a project
(define-public (add-project-approver (project-id (string-ascii 64)) (approver principal))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    
    (map-set project-approvers { project-id: project-id, approver: approver } true)
    
    (record-event "APPROVER_ADDED" (some project-id) none (some (principal->string approver)))
    
    (ok true)
  )
)

;; Remove an approver from a project
(define-public (remove-project-approver (project-id (string-ascii 64)) (approver principal))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    
    (map-set project-approvers { project-id: project-id, approver: approver } false)
    
    (record-event "APPROVER_REMOVED" (some project-id) none (some (principal->string approver)))
    
    (ok true)
  )
)

;; Update project allocation
(define-public (update-project-allocation (project-id (string-ascii 64)) (new-allocation uint))
  (begin
    (asserts! (is-contract-admin) ERR-NOT-AUTHORIZED)
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (asserts! (not (is-project-locked project-id)) ERR-PROJECT-LOCKED)
    
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id })))
          (current-spent (get spent-amount project))
          (current-allocation (get allocated-amount project)))
      
      ;; Ensure new allocation covers already spent amount
      (asserts! (>= new-allocation current-spent) ERR-INVALID-AMOUNT)
      
      ;; Ensure treasury can cover additional allocation if increasing
      (if (> new-allocation current-allocation)
          (asserts! (<= (- new-allocation current-allocation) (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
          true)
      
      ;; Update project allocation
      (map-set projects
        { project-id: project-id }
        (merge project { allocated-amount: new-allocation })
      )
      
      (record-event "ALLOCATION_UPDATED" (some project-id) (some new-allocation) none)
      
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
      (map-set projects
        { project-id: project-id }
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
      (map-set projects
        { project-id: project-id }
        (merge project { locked: false })
      )
      
      (record-event "PROJECT_UNLOCKED" (some project-id) none none)
      
      (ok true)
    )
  )
)

;; Propose a withdrawal from a project
(define-public (propose-withdrawal (project-id (string-ascii 64)) (amount uint) (recipient principal))
  (begin
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (asserts! (is-project-approver project-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (let ((project (unwrap-panic (map-get? projects { project-id: project-id })))
          (project-balance (- (get allocated-amount project) (get spent-amount project))))
      
      ;; Check that project has enough unspent funds
      (asserts! (>= project-balance amount) ERR-INSUFFICIENT-FUNDS)
      
      ;; Create new withdrawal proposal with a new ID
      (let ((withdrawal-id (increment-withdrawal-counter project-id)))
        (map-set withdrawal-approvals
          { project-id: project-id, withdrawal-id: withdrawal-id }
          {
            amount: amount,
            recipient: recipient,
            approvers: (list tx-sender),  ;; Proposer is first approver
            executed: false
          }
        )
        
        (record-event "WITHDRAWAL_PROPOSED" 
                     (some project-id) 
                     (some amount) 
                     (some (concat "Withdrawal ID: " (uint->string withdrawal-id))))
        
        ;; If only admin approval needed and proposer is admin, execute immediately
        (if (and (is-eq (get approval-threshold project) u1) (is-contract-admin))
            (execute-withdrawal project-id withdrawal-id)
            (ok withdrawal-id))
      )
    )
  )
)

;; Approve a withdrawal proposal
(define-public (approve-withdrawal (project-id (string-ascii 64)) (withdrawal-id uint))
  (begin
    (asserts! (project-exists project-id) ERR-UNKNOWN-PROJECT)
    (asserts! (is-project-approver project-id tx-sender) ERR-NOT-AUTHORIZED)
    
    (let ((withdrawal (unwrap! (map-get? withdrawal-approvals 
                               { project-id: project-id, withdrawal-id: withdrawal-id }) 
                     ERR-UNKNOWN-PROJECT)))
      
      ;; Check that withdrawal hasn't been executed and approver hasn't already approved
      (asserts! (not (get executed withdrawal)) ERR-INVALID-AMOUNT)
      (asserts! (is-none (index-of (get approvers withdrawal) tx-sender)) ERR-NOT-AUTHORIZED)
      
      ;; Add approver to the list
      (let ((updated-approvers (append (get approvers withdrawal) tx-sender)))
        (map-set withdrawal-approvals
          { project-id: project-id, withdrawal-id: withdrawal-id }
          (merge withdrawal { approvers: updated-approvers })
        )
        
        (record-event "WITHDRAWAL_APPROVED" 
                     (some project-id) 
                     none 
                     (some (concat "Withdrawal ID: " (uint->string withdrawal-id))))
        
        ;; Check if threshold is met after this approval
        (let ((project (unwrap-panic (map-get? projects { project-id: project-id })))
              (approvers-count (len updated-approvers)))
          
          (if (>= approvers-count (get approval-threshold project))
              (execute-withdrawal project-id withdrawal-id)
              (ok withdrawal-id))
        )
      )
    )
  )
)

;; Execute a withdrawal when thresholds are met
(define-private (execute-withdrawal (project-id (string-ascii 64)) (withdrawal-id uint))
  (let ((withdrawal (unwrap! (map-get? withdrawal-approvals 
                             { project-id: project-id, withdrawal-id: withdrawal-id }) 
                   ERR-UNKNOWN-PROJECT))
        (project (unwrap! (map-get? projects { project-id: project-id }) ERR-UNKNOWN-PROJECT)))
    
    ;; Check that withdrawal hasn't been executed
    (asserts! (not (get executed withdrawal)) ERR-INVALID-AMOUNT)
    
    ;; Check that approval threshold is met
    (asserts! (>= (len (get approvers withdrawal)) (get approval-threshold project)) ERR-THRESHOLD-NOT-MET)
    
    ;; Check that project has enough unspent funds
    (let ((amount (get amount withdrawal))
          (project-balance (- (get allocated-amount project) (get spent-amount project))))
      (asserts! (>= project-balance amount) ERR-INSUFFICIENT-FUNDS)
      
      ;; Update withdrawal as executed
      (map-set withdrawal-approvals
        { project-id: project-id, withdrawal-id: withdrawal-id }
        (merge withdrawal { executed: true })
      )
      
      ;; Update project spent amount
      (map-set projects
        { project-id: project-id }
        (merge project { spent-amount: (+ (get spent-amount project) amount) })
      )
      
      ;; Transfer funds to recipient
      (try! (as-contract (stx-transfer? amount tx-sender (get recipient withdrawal))))
      
      ;; Update treasury balance
      (var-set treasury-balance (- (var-get treasury-balance) amount))
      
      ;; Record event
      (record-event "WITHDRAWAL_EXECUTED" 
                   (some project-id) 
                   (some amount) 
                   (some (concat "Recipient: " (principal->string (get recipient withdrawal)))))
      
      (ok withdrawal-id)
    )
  )
)