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
  {
    pool-id: uint,
    community-id: (optional principal),
  }
  {
    balance: uint, ;; Available funds in the pool (in uSTX)
    total-distributed: uint, ;; Total funds distributed from this pool
    reward-model: (string-ascii 20), ;; "fixed", "proportional", or "reputation"
    is-active: bool, ;; Whether the pool is active
    controller: principal, ;; Address with admin rights to the pool
    locked-until: uint, ;; Block height until which pool is locked (0 if not locked)
  }
)
;; Tracks tasks and their associated rewards
(define-map tasks
  { task-id: uint }
  {
    pool-id: uint, ;; The reward pool this task belongs to
    reward-amount: uint, ;; Fixed reward amount (for fixed reward model)
    reward-percentage: uint, ;; Percentage of pool (for proportional model, basis points 1/100 of 1%)
    status: (string-ascii 20), ;; "pending", "completed", "cancelled"
    assigned-to: (optional principal), ;; Address assigned to complete the task
    completed-at: (optional uint), ;; Block height when task was marked complete
    reward-claimed: bool, ;; Whether the reward has been claimed
    verifier: (optional principal), ;; Address that can verify task completion
  }
)
;; Tracks user reputation within communities for reputation-based rewards
(define-map user-reputation
  {
    user: principal,
    community-id: (optional principal),
  }
  {
    score: uint, ;; Reputation score (0-10000, representing 0-100.00%)
    tasks-completed: uint, ;; Number of tasks completed
    total-earned: uint, ;; Total rewards earned
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
(define-private (is-pool-controller
    (pool-id uint)
    (community-id (optional principal))
  )
  (match (map-get? reward-pools {
    pool-id: pool-id,
    community-id: community-id,
  })
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

;; Read-only functions
;; Get reward pool details
(define-read-only (get-reward-pool
    (pool-id uint)
    (community-id (optional principal))
  )
  (map-get? reward-pools {
    pool-id: pool-id,
    community-id: community-id,
  })
)

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

;; Get user reputation in a community
(define-read-only (get-user-reputation
    (user principal)
    (community-id (optional principal))
  )
  (default-to {
    score: u0,
    tasks-completed: u0,
    total-earned: u0,
  }
    (map-get? user-reputation {
      user: user,
      community-id: community-id,
    })
  )
)

;; Public functions
;; Create a new reward pool
(define-public (create-reward-pool
    (initial-balance uint)
    (reward-model (string-ascii 20))
    (community-id (optional principal))
  )
  (let ((pool-id (get-and-increment-pool-id)))
    ;; Check valid parameters
    (asserts! (> initial-balance u0) ERR-INVALID-PARAMS)
    (asserts! (is-valid-reward-model reward-model) ERR-INVALID-PARAMS)
    ;; Create the pool
    (map-set reward-pools {
      pool-id: pool-id,
      community-id: community-id,
    } {
      balance: initial-balance,
      total-distributed: u0,
      reward-model: reward-model,
      is-active: true,
      controller: tx-sender,
      locked-until: u0,
    })
    (ok pool-id)
  )
)

;; Create a new task with reward
(define-public (create-task
    (pool-id uint)
    (reward-amount uint)
    (reward-percentage uint)
    (assigned-to (optional principal))
    (verifier (optional principal))
    (community-id (optional principal))
  )
  (let ((task-id (get-and-increment-task-id)))
    ;; Check pool exists and caller is authorized
    (asserts! (is-pool-controller pool-id community-id) ERR-NOT-AUTHORIZED)
    ;; Create the task
    (map-set tasks { task-id: task-id } {
      pool-id: pool-id,
      reward-amount: reward-amount,
      reward-percentage: reward-percentage,
      status: "pending",
      assigned-to: assigned-to,
      completed-at: none,
      reward-claimed: false,
      verifier: verifier,
    })
    (ok task-id)
  )
)

;; Mark a task as completed
(define-public (complete-task (task-id uint))
  (match (map-get? tasks { task-id: task-id })
    task-data (begin
      ;; Check task is assigned to sender or sender is verifier
      (asserts!
        (or
          (is-eq (some tx-sender) (get assigned-to task-data))
          (is-eq (some tx-sender) (get verifier task-data))
        )
        ERR-NOT-AUTHORIZED
      )
      ;; Check task is pending
      (asserts! (is-eq (get status task-data) "pending") ERR-INVALID-PARAMS)
      ;; Update task status
      (map-set tasks { task-id: task-id }
        (merge task-data {
          status: "completed",
          completed-at: (some block-height),
        })
      )
      (ok true)
    )
    ERR-TASK-NOT-FOUND
  )
)