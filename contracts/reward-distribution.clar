;; reward-distribution
;; 
;; This contract manages the allocation and distribution of rewards for completed tasks
;; within community organizations. It enables communities to fund projects, set reward parameters,
;; and automate payments upon task verification, supporting various compensation models.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARAMS (err u101))
(define-constant ERR-REWARD-POOL-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-TASK-NOT-FOUND (err u104))
(define-constant ERR-TASK-NOT-COMPLETED (err u105))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u106))
(define-constant ERR-INVALID-RECIPIENT (err u107))
(define-constant ERR-ZERO-AMOUNT (err u108))
(define-constant ERR-POOL-LOCKED (err u109))

;; Data space definitions

;; Tracks each community's reward pools with associated metadata
(define-map reward-pools
  { pool-id: uint, community-id: (optional principal) }
  {
    balance: uint,                 ;; Available funds in the pool (in uSTX)
    total-distributed: uint,       ;; Total funds distributed from this pool
    reward-model: (string-ascii 20), ;; "fixed", "proportional", or "reputation"
    is-active: bool,               ;; Whether the pool is active
    controller: principal,         ;; Address with admin rights to the pool
    locked-until: uint             ;; Block height until which pool is locked (0 if not locked)
  }
)

;; Tracks tasks and their associated rewards
(define-map tasks
  { task-id: uint }
  {
    pool-id: uint,                 ;; The reward pool this task belongs to
    reward-amount: uint,           ;; Fixed reward amount (for fixed reward model)
    reward-percentage: uint,       ;; Percentage of pool (for proportional model, basis points 1/100 of 1%)
    status: (string-ascii 20),     ;; "pending", "completed", "cancelled"
    assigned-to: (optional principal), ;; Address assigned to complete the task
    completed-at: (optional uint), ;; Block height when task was marked complete
    reward-claimed: bool,          ;; Whether the reward has been claimed
    verifier: (optional principal) ;; Address that can verify task completion
  }
)

;; Tracks user reputation within communities for reputation-based rewards
(define-map user-reputation
  { user: principal, community-id: (optional principal) }
  { 
    score: uint,                   ;; Reputation score (0-10000, representing 0-100.00%)
    tasks-completed: uint,         ;; Number of tasks completed
    total-earned: uint             ;; Total rewards earned
  }
)

;; Incrementing counters
(define-data-var next-pool-id uint u1)
(define-data-var next-task-id uint u1)

;; Private functions

;; Get the current reward pool ID and increment for next use
(define-private (get-and-increment-pool-id)
  (let ((current-id (var-get next-pool-id)))
    (var-set next-pool-id (+ current-id u1))
    current-id
  )
)

;; Get the current task ID and increment for next use
(define-private (get-and-increment-task-id)
  (let ((current-id (var-get next-task-id)))
    (var-set next-task-id (+ current-id u1))
    current-id
  )
)

;; Check if sender is authorized to manage a specific reward pool
(define-private (is-pool-controller (pool-id uint) (community-id (optional principal)))
  (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
    pool (is-eq (get controller pool) tx-sender)
    false
  )
)

;; Validate reward model string
(define-private (is-valid-reward-model (model (string-ascii 20)))
  (or
    (is-eq model "fixed")
    (is-eq model "proportional")
    (is-eq model "reputation")
  )
)

;; Calculate reward amount based on reward model
(define-private (calculate-reward
                  (task-map {
                    pool-id: uint,
                    reward-amount: uint,
                    reward-percentage: uint,
                    status: (string-ascii 20),
                    assigned-to: (optional principal),
                    completed-at: (optional uint),
                    reward-claimed: bool,
                    verifier: (optional principal)
                  })
                  (pool-map {
                    balance: uint,
                    total-distributed: uint,
                    reward-model: (string-ascii 20),
                    is-active: bool,
                    controller: principal,
                    locked-until: uint
                  })
                  (recipient principal)
                  (community-id (optional principal)))
  (let ((reward-model (get reward-model pool-map)))
    (cond
      ;; Fixed reward model - simply return the fixed amount
      (is-eq reward-model "fixed") (ok (get reward-amount task-map))
      
      ;; Proportional reward model - calculate percentage of available pool
      (is-eq reward-model "proportional")
        (ok (/ (* (get balance pool-map) (get reward-percentage task-map)) u10000))
      
      ;; Reputation-based reward model
      (is-eq reward-model "reputation")
        (match (map-get? user-reputation { user: recipient, community-id: community-id })
          reputation-data 
            (let ((reputation-score (get score reputation-data)))
              ;; Base amount plus reputation-based bonus
              (ok (+ (get reward-amount task-map) 
                    (/ (* (get reward-amount task-map) reputation-score) u10000))))
          ;; If no reputation data, just return base amount
          (ok (get reward-amount task-map)))
      
      ;; Invalid reward model
      (err u101)
    )
  )
)

;; Update user reputation after completing a task
(define-private (update-reputation (user principal) (community-id (optional principal)) (reward-amount uint))
  (let ((user-data (default-to 
                      { score: u0, tasks-completed: u0, total-earned: u0 }
                      (map-get? user-reputation { user: user, community-id: community-id }))))
    ;; Increase tasks completed count and total earned
    (map-set user-reputation
      { user: user, community-id: community-id }
      { 
        score: (min u10000 (+ (get score user-data) u100)), ;; Increment score by 1% (capped at 100%)
        tasks-completed: (+ (get tasks-completed user-data) u1),
        total-earned: (+ (get total-earned user-data) reward-amount)
      }
    )
  )
)

;; Read-only functions

;; Get reward pool details
(define-read-only (get-reward-pool (pool-id uint) (community-id (optional principal)))
  (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
)

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

;; Get user reputation in a community
(define-read-only (get-user-reputation (user principal) (community-id (optional principal)))
  (default-to 
    { score: u0, tasks-completed: u0, total-earned: u0 }
    (map-get? user-reputation { user: user, community-id: community-id })
  )
)

;; Check if a task's reward can be claimed
(define-read-only (is-reward-claimable (task-id uint))
  (match (map-get? tasks { task-id: task-id })
    task (and 
            (is-eq (get status task) "completed")
            (not (get reward-claimed task))
            (match (map-get? reward-pools 
                    { pool-id: (get pool-id task), 
                      community-id: none })
              pool (and
                     (get is-active pool)
                     (>= block-height (get locked-until pool))
                     (>= (get balance pool) 
                         (default-to u0 
                           (calculate-reward 
                             task 
                             pool
                             (default-to tx-sender (get assigned-to task))
                             none))))
              false))
    false)
)

;; Public functions

;; Create a new reward pool
(define-public (create-reward-pool 
                (initial-balance uint) 
                (reward-model (string-ascii 20))
                (community-id (optional principal)))
  (let ((pool-id (get-and-increment-pool-id)))
    ;; Check valid parameters
    (asserts! (> initial-balance u0) ERR-INVALID-PARAMS)
    (asserts! (is-valid-reward-model reward-model) ERR-INVALID-PARAMS)
    
    ;; Transfer funds to contract
    (asserts! (stx-transfer? initial-balance tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create the pool
    (map-set reward-pools
      { pool-id: pool-id, community-id: community-id }
      {
        balance: initial-balance,
        total-distributed: u0,
        reward-model: reward-model,
        is-active: true,
        controller: tx-sender,
        locked-until: u0
      }
    )
    
    (ok pool-id)
  )
)

;; Fund an existing reward pool
(define-public (fund-reward-pool (pool-id uint) (amount uint) (community-id (optional principal)))
  (asserts! (> amount u0) ERR-ZERO-AMOUNT)
  
  (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
    pool 
      (begin
        ;; Transfer funds to contract
        (asserts! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Update pool balance
        (map-set reward-pools
          { pool-id: pool-id, community-id: community-id }
          (merge pool { balance: (+ (get balance pool) amount) })
        )
        
        (ok true)
      )
    ERR-REWARD-POOL-NOT-FOUND
  )
)

;; Create a new task with reward
(define-public (create-task 
                (pool-id uint) 
                (reward-amount uint)
                (reward-percentage uint) 
                (assigned-to (optional principal))
                (verifier (optional principal))
                (community-id (optional principal)))
  (let ((task-id (get-and-increment-task-id)))
    ;; Check pool exists and caller is authorized
    (asserts! (is-pool-controller pool-id community-id) ERR-NOT-AUTHORIZED)
    
    ;; Create the task
    (map-set tasks
      { task-id: task-id }
      {
        pool-id: pool-id,
        reward-amount: reward-amount,
        reward-percentage: reward-percentage,
        status: "pending",
        assigned-to: assigned-to,
        completed-at: none,
        reward-claimed: false,
        verifier: verifier
      }
    )
    
    (ok task-id)
  )
)

;; Mark a task as completed
(define-public (complete-task (task-id uint))
  (match (map-get? tasks { task-id: task-id })
    task-data
      (begin
        ;; Check task is assigned to sender or sender is verifier
        (asserts! (or
                    (is-eq (some tx-sender) (get assigned-to task-data))
                    (is-eq (some tx-sender) (get verifier task-data))
                  ) ERR-NOT-AUTHORIZED)
        
        ;; Check task is pending
        (asserts! (is-eq (get status task-data) "pending") ERR-INVALID-PARAMS)
        
        ;; Update task status
        (map-set tasks
          { task-id: task-id }
          (merge task-data 
            { 
              status: "completed",
              completed-at: (some block-height)
            }
          )
        )
        
        (ok true)
      )
    ERR-TASK-NOT-FOUND
  )
)

;; Claim reward for a completed task
(define-public (claim-reward (task-id uint))
  (match (map-get? tasks { task-id: task-id })
    task-data
      (let (
            (pool-id (get pool-id task-data))
            (community-id none)
            (reward-recipient (default-to tx-sender (get assigned-to task-data)))
          )
        
        ;; Check sender is either assigned to task or is verifier
        (asserts! (or
                    (is-eq tx-sender reward-recipient)
                    (is-eq (some tx-sender) (get verifier task-data))
                  ) ERR-NOT-AUTHORIZED)
        
        ;; Check task is completed
        (asserts! (is-eq (get status task-data) "completed") ERR-TASK-NOT-COMPLETED)
        
        ;; Check reward hasn't been claimed already
        (asserts! (not (get reward-claimed task-data)) ERR-REWARD-ALREADY-CLAIMED)
        
        (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
          pool-data
            (begin
              ;; Check pool is active and not locked
              (asserts! (get is-active pool-data) ERR-POOL-LOCKED)
              (asserts! (>= block-height (get locked-until pool-data)) ERR-POOL-LOCKED)
              
              ;; Calculate reward based on model
              (match (calculate-reward task-data pool-data reward-recipient community-id)
                reward-amount
                  (begin
                    ;; Check sufficient funds in pool
                    (asserts! (>= (get balance pool-data) reward-amount) ERR-INSUFFICIENT-FUNDS)
                    
                    ;; Mark reward as claimed
                    (map-set tasks
                      { task-id: task-id }
                      (merge task-data { reward-claimed: true })
                    )
                    
                    ;; Update pool balance
                    (map-set reward-pools
                      { pool-id: pool-id, community-id: community-id }
                      (merge pool-data 
                        { 
                          balance: (- (get balance pool-data) reward-amount),
                          total-distributed: (+ (get total-distributed pool-data) reward-amount)
                        }
                      )
                    )
                    
                    ;; Update recipient's reputation
                    (update-reputation reward-recipient community-id reward-amount)
                    
                    ;; Transfer funds to recipient
                    (as-contract (stx-transfer? reward-amount tx-sender reward-recipient))
                  )
                err ERR-INVALID-PARAMS
              )
            )
          ERR-REWARD-POOL-NOT-FOUND
        )
      )
    ERR-TASK-NOT-FOUND
  )
)

;; Update pool settings (only authorized controller)
(define-public (update-pool-settings 
                (pool-id uint) 
                (is-active bool) 
                (locked-until uint)
                (community-id (optional principal)))
  ;; Check pool exists and caller is authorized
  (asserts! (is-pool-controller pool-id community-id) ERR-NOT-AUTHORIZED)
  
  (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
    pool-data
      (begin
        ;; Update pool settings
        (map-set reward-pools
          { pool-id: pool-id, community-id: community-id }
          (merge pool-data 
            { 
              is-active: is-active,
              locked-until: locked-until
            }
          )
        )
        
        (ok true)
      )
    ERR-REWARD-POOL-NOT-FOUND
  )
)

;; Transfer pool control to new address
(define-public (transfer-pool-control 
                (pool-id uint) 
                (new-controller principal)
                (community-id (optional principal)))
  ;; Check pool exists and caller is authorized
  (asserts! (is-pool-controller pool-id community-id) ERR-NOT-AUTHORIZED)
  
  (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
    pool-data
      (begin
        ;; Update pool controller
        (map-set reward-pools
          { pool-id: pool-id, community-id: community-id }
          (merge pool-data { controller: new-controller })
        )
        
        (ok true)
      )
    ERR-REWARD-POOL-NOT-FOUND
  )
)

;; Withdraw funds from a pool (only authorized controller)
(define-public (withdraw-from-pool 
                (pool-id uint) 
                (amount uint) 
                (recipient principal)
                (community-id (optional principal)))
  ;; Check pool exists and caller is authorized
  (asserts! (is-pool-controller pool-id community-id) ERR-NOT-AUTHORIZED)
  (asserts! (> amount u0) ERR-ZERO-AMOUNT)
  
  (match (map-get? reward-pools { pool-id: pool-id, community-id: community-id })
    pool-data
      (begin
        ;; Check sufficient funds
        (asserts! (>= (get balance pool-data) amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Update pool balance
        (map-set reward-pools
          { pool-id: pool-id, community-id: community-id }
          (merge pool-data { balance: (- (get balance pool-data) amount) })
        )
        
        ;; Transfer funds to recipient
        (as-contract (stx-transfer? amount tx-sender recipient))
      )
    ERR-REWARD-POOL-NOT-FOUND
  )
)