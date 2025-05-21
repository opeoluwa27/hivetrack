;; task-tracking.clar
;; Author: HiveTrack Team
;; =========================================
;; CONSTANTS
;; =========================================
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-TASK-EXISTS (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-PRIORITY (err u104))
(define-constant ERR-INVALID-ASSIGNMENT (err u105))
(define-constant ERR-CANNOT-MODIFY (err u106))
(define-constant ERR-MILESTONE-NOT-FOUND (err u107))
(define-constant ERR-CATEGORY-NOT-FOUND (err u108))
(define-constant ERR-INVALID-CATEGORY (err u109))
(define-constant ERR-GOAL-NOT-FOUND (err u110))
;; Task status values
(define-constant STATUS-CREATED u1)
(define-constant STATUS-ASSIGNED u2)
(define-constant STATUS-IN-PROGRESS u3)
(define-constant STATUS-UNDER-REVIEW u4)
(define-constant STATUS-COMPLETED u5)
(define-constant STATUS-CANCELLED u6)
;; Priority levels
(define-constant PRIORITY-LOW u1)
(define-constant PRIORITY-MEDIUM u2)
(define-constant PRIORITY-HIGH u3)
(define-constant PRIORITY-URGENT u4)
;; =========================================
;; DATA MAPS AND VARIABLES
;; =========================================
;; Tracks the total number of tasks created
(define-data-var task-counter uint u0)
;; Main task data structure
(define-map tasks
  { task-id: uint }
  {
    title: (string-ascii 64),
    description: (string-utf8 500),
    creator: principal,
    assignee: (optional principal),
    status: uint,
    priority: uint,
    category: (optional uint),
    goal-id: (optional uint),
    created-at: uint,
    updated-at: uint,
    deadline: (optional uint),
  }
)
;; Stores task milestones and progress
(define-map task-milestones
  {
    task-id: uint,
    milestone-id: uint,
  }
  {
    description: (string-utf8 200),
    completed: bool,
    completed-by: (optional principal),
    completed-at: (optional uint),
    verification-data: (optional (string-utf8 500)),
  }
)
;; Tracks the number of milestones per task
(define-map task-milestone-counter
  { task-id: uint }
  { count: uint }
)
;; Categories for organizing tasks
(define-map categories
  { category-id: uint }
  {
    name: (string-ascii 32),
    description: (string-utf8 200),
    created-by: principal,
  }
)
;; Tracks the total number of categories
(define-data-var category-counter uint u0)
;; Community goals that tasks can be linked to
(define-map goals
  { goal-id: uint }
  {
    title: (string-ascii 64),
    description: (string-utf8 300),
    created-by: principal,
    created-at: uint,
    target-date: (optional uint),
  }
)
;; Tracks the total number of goals
(define-data-var goal-counter uint u0)
;; =========================================
;; PRIVATE FUNCTIONS
;; =========================================
;; Validates task status transitions
(define-private (is-valid-status-transition
    (current-status uint)
    (new-status uint)
  )
  (or
    ;; Created -> Assigned, Cancelled
    (and
      (is-eq current-status STATUS-CREATED)
      (or
        (is-eq new-status STATUS-ASSIGNED)
        (is-eq new-status STATUS-CANCELLED)
      )
    )
    ;; Assigned -> In Progress, Cancelled
    (and
      (is-eq current-status STATUS-ASSIGNED)
      (or
        (is-eq new-status STATUS-IN-PROGRESS)
        (is-eq new-status STATUS-CANCELLED)
      )
    )
    ;; In Progress -> Under Review, Cancelled
    (and
      (is-eq current-status STATUS-IN-PROGRESS)
      (or
        (is-eq new-status STATUS-UNDER-REVIEW)
        (is-eq new-status STATUS-CANCELLED)
      )
    )
    ;; Under Review -> In Progress, Completed, Cancelled
    (and
      (is-eq current-status STATUS-UNDER-REVIEW)
      (or
        (is-eq new-status STATUS-IN-PROGRESS)
        (is-eq new-status STATUS-COMPLETED)
        (is-eq new-status STATUS-CANCELLED)
      )
    )
  )
)

;; Validates priority value
(define-private (is-valid-priority (priority uint))
  (and (>= priority PRIORITY-LOW) (<= priority PRIORITY-URGENT))
)

;; Gets the current task milestone counter or defaults to 0
(define-private (get-milestone-count (task-id uint))
  (default-to { count: u0 }
    (map-get? task-milestone-counter { task-id: task-id })
  )
)

;; Checks if the task exists
(define-private (task-exists (task-id uint))
  (is-some (map-get? tasks { task-id: task-id }))
)

;; Checks if the category exists
(define-private (category-exists (category-id uint))
  (is-some (map-get? categories { category-id: category-id }))
)

;; Checks if the goal exists
(define-private (goal-exists (goal-id uint))
  (is-some (map-get? goals { goal-id: goal-id }))
)

;; Checks if the principal is the creator or assignee of a task
(define-private (is-task-participant
    (task-id uint)
    (user principal)
  )
  (match (map-get? tasks { task-id: task-id })
    task (or
      (is-eq (get creator task) user)
      (match (get assignee task)
        assignee (is-eq assignee user)
        false
      )
    )
    false
  )
)

;; =========================================
;; READ-ONLY FUNCTIONS
;; =========================================
;; Gets details of a specific task
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

;; Gets a milestone for a specific task
(define-read-only (get-task-milestone
    (task-id uint)
    (milestone-id uint)
  )
  (map-get? task-milestones {
    task-id: task-id,
    milestone-id: milestone-id,
  })
)

;; Gets details of a specific category
(define-read-only (get-category (category-id uint))
  (map-get? categories { category-id: category-id })
)

;; Gets details of a specific goal
(define-read-only (get-goal (goal-id uint))
  (map-get? goals { goal-id: goal-id })
)

;; Gets the total number of tasks
(define-read-only (get-task-count)
  (var-get task-counter)
)

;; Gets the total number of categories
(define-read-only (get-category-count)
  (var-get category-counter)
)

;; Gets the total number of goals
(define-read-only (get-goal-count)
  (var-get goal-counter)
)

;; =========================================
;; PUBLIC FUNCTIONS
;; =========================================
;; Creates a new task
(define-public (create-task
    (title (string-ascii 64))
    (description (string-utf8 500))
    (priority uint)
    (category (optional uint))
    (goal-id (optional uint))
    (deadline (optional uint))
  )
  (let (
      (task-id (+ (var-get task-counter) u1))
      (current-time (unwrap-panic (get-block-info? time block-height)))
    )
    ;; Check priority is valid
    (asserts! (is-valid-priority priority) ERR-INVALID-PRIORITY)
    ;; Check category exists if provided
    (asserts!
      (match category
        category-id (category-exists category-id)
        true
      )
      ERR-CATEGORY-NOT-FOUND
    )
    ;; Check goal exists if provided
    (asserts! (match goal-id
      goal (goal-exists goal)
      true
    )
      ERR-GOAL-NOT-FOUND
    )
    ;; Create the task
    (map-set tasks { task-id: task-id } {
      title: title,
      description: description,
      creator: tx-sender,
      assignee: none,
      status: STATUS-CREATED,
      priority: priority,
      category: category,
      goal-id: goal-id,
      created-at: current-time,
      updated-at: current-time,
      deadline: deadline,
    })
    ;; Initialize milestone counter for this task
    (map-set task-milestone-counter { task-id: task-id } { count: u0 })
    ;; Increment task counter
    (var-set task-counter task-id)
    (ok task-id)
  )
)

;; Assigns a task to a principal
(define-public (assign-task
    (task-id uint)
    (assignee principal)
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is the creator or current assignee
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check task is in a valid state for assignment
        (asserts! (is-eq (get status task) STATUS-CREATED) ERR-INVALID-STATUS)
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            assignee: (some assignee),
            status: STATUS-ASSIGNED,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Updates the status of a task
(define-public (update-task-status
    (task-id uint)
    (new-status uint)
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check status transition is valid
        (asserts! (is-valid-status-transition (get status task) new-status)
          ERR-INVALID-STATUS
        )
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            status: new-status,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Updates the priority of a task
(define-public (update-task-priority
    (task-id uint)
    (new-priority uint)
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check priority is valid
        (asserts! (is-valid-priority new-priority) ERR-INVALID-PRIORITY)
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            priority: new-priority,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Adds a milestone to a task
(define-public (add-task-milestone
    (task-id uint)
    (description (string-utf8 200))
  )
  (match (map-get? tasks { task-id: task-id })
    task (let (
        (milestone-count (get count (get-milestone-count task-id)))
        (new-milestone-id milestone-count)
      )
      ;; Check sender is task participant
      (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
      ;; Add the milestone
      (map-set task-milestones {
        task-id: task-id,
        milestone-id: new-milestone-id,
      } {
        description: description,
        completed: false,
        completed-by: none,
        completed-at: none,
        verification-data: none,
      })
      ;; Increment milestone counter
      (map-set task-milestone-counter { task-id: task-id } { count: (+ milestone-count u1) })
      (ok new-milestone-id)
    )
    ERR-TASK-NOT-FOUND
  )
)

;; Completes a task milestone
(define-public (complete-milestone
    (task-id uint)
    (milestone-id uint)
    (verification-data (optional (string-utf8 500)))
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check milestone exists
        (asserts!
          (is-some (map-get? task-milestones {
            task-id: task-id,
            milestone-id: milestone-id,
          }))
          ERR-MILESTONE-NOT-FOUND
        )
        ;; Complete the milestone
        (match (map-get? task-milestones {
          task-id: task-id,
          milestone-id: milestone-id,
        })
          milestone (begin
            ;; Check milestone isn't already completed
            (asserts! (not (get completed milestone)) ERR-CANNOT-MODIFY)
            (map-set task-milestones {
              task-id: task-id,
              milestone-id: milestone-id,
            }
              (merge milestone {
                completed: true,
                completed-by: (some tx-sender),
                completed-at: (some current-time),
                verification-data: verification-data,
              })
            )
            (ok true)
          )
          ERR-MILESTONE-NOT-FOUND
        )
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Creates a new category
(define-public (create-category
    (name (string-ascii 32))
    (description (string-utf8 200))
  )
  (let ((category-id (+ (var-get category-counter) u1)))
    ;; Create the category
    (map-set categories { category-id: category-id } {
      name: name,
      description: description,
      created-by: tx-sender,
    })
    ;; Increment category counter
    (var-set category-counter category-id)
    (ok category-id)
  )
)

;; Creates a new goal
(define-public (create-goal
    (title (string-ascii 64))
    (description (string-utf8 300))
    (target-date (optional uint))
  )
  (let (
      (goal-id (+ (var-get goal-counter) u1))
      (current-time (unwrap-panic (get-block-info? time block-height)))
    )
    ;; Create the goal
    (map-set goals { goal-id: goal-id } {
      title: title,
      description: description,
      created-by: tx-sender,
      created-at: current-time,
      target-date: target-date,
    })
    ;; Increment goal counter
    (var-set goal-counter goal-id)
    (ok goal-id)
  )
)

;; Update task description
(define-public (update-task-description
    (task-id uint)
    (new-description (string-utf8 500))
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check task is not completed or cancelled
        (asserts!
          (and
            (not (is-eq (get status task) STATUS-COMPLETED))
            (not (is-eq (get status task) STATUS-CANCELLED))
          )
          ERR-CANNOT-MODIFY
        )
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            description: new-description,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Update task deadline
(define-public (update-task-deadline
    (task-id uint)
    (new-deadline (optional uint))
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check task is not completed or cancelled
        (asserts!
          (and
            (not (is-eq (get status task) STATUS-COMPLETED))
            (not (is-eq (get status task) STATUS-CANCELLED))
          )
          ERR-CANNOT-MODIFY
        )
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            deadline: new-deadline,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Link task to category
(define-public (link-task-to-category
    (task-id uint)
    (category-id (optional uint))
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check category exists if provided
        (asserts!
          (match category-id
            category (category-exists category)
            true
          )
          ERR-CATEGORY-NOT-FOUND
        )
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            category: category-id,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)

;; Link task to goal
(define-public (link-task-to-goal
    (task-id uint)
    (goal-id (optional uint))
  )
  (let ((current-time (unwrap-panic (get-block-info? time block-height))))
    (match (map-get? tasks { task-id: task-id })
      task (begin
        ;; Check sender is task participant
        (asserts! (is-task-participant task-id tx-sender) ERR-NOT-AUTHORIZED)
        ;; Check goal exists if provided
        (asserts!
          (match goal-id
            goal (goal-exists goal)
            true
          )
          ERR-GOAL-NOT-FOUND
        )
        ;; Update the task
        (map-set tasks { task-id: task-id }
          (merge task {
            goal-id: goal-id,
            updated-at: current-time,
          })
        )
        (ok true)
      )
      ERR-TASK-NOT-FOUND
    )
  )
)
