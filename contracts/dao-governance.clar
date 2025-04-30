;; dao-governance
;; This contract forms the core of the hivetrack system, enabling users to create and manage
;; decentralized community organizations with customizable governance parameters.
;; It handles membership management, voting rights allocation, and proposal mechanisms.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARAMETER (err u101))
(define-constant ERR-DAO-NOT-FOUND (err u102))
(define-constant ERR-MEMBER-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-MEMBER (err u104))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-PROPOSAL-EXPIRED (err u107))
(define-constant ERR-PROPOSAL-ACTIVE (err u108))
(define-constant ERR-INSUFFICIENT-VOTES (err u109))
(define-constant ERR-NOT-MEMBER (err u110))

;; Data structures

;; DAO struct: Stores details about each community organization
(define-map daos
  { dao-id: uint }
  {
    name: (string-ascii 64),
    description: (string-utf8 256),
    creator: principal,
    created-at: uint,
    voting-threshold: uint,        ;; Percentage (1-100) of votes needed to pass a proposal
    voting-period: uint,           ;; Duration of voting period in blocks
    membership-type: (string-ascii 10),  ;; "open" or "invite" or "token"
    member-count: uint
  }
)

;; Membership management for each DAO
(define-map dao-members
  { dao-id: uint, member: principal }
  {
    joined-at: uint,
    voting-power: uint,            ;; Voting weight for this member
    role: (string-ascii 16)        ;; "admin", "member", etc.
  }
)

;; Proposals tracking
(define-map proposals
  { dao-id: uint, proposal-id: uint }
  {
    title: (string-ascii 64),
    description: (string-utf8 256),
    proposer: principal,
    created-at: uint,
    expires-at: uint,
    executed: bool,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 10)      ;; "active", "passed", "rejected", "executed"
  }
)

;; Track votes cast by members
(define-map votes
  { dao-id: uint, proposal-id: uint, voter: principal }
  {
    vote: bool,                    ;; true = for, false = against
    weight: uint,                  ;; Voting power used
    voted-at: uint
  }
)

;; Data variables
(define-data-var next-dao-id uint u1)
(define-data-var next-proposal-id-by-dao (map uint uint) (map))

;; Private functions

;; Get the next proposal ID for a specific DAO
(define-private (get-next-proposal-id (dao-id uint))
  (default-to u1 (map-get? next-proposal-id-by-dao dao-id))
)

;; Update the next proposal ID for a specific DAO
(define-private (update-next-proposal-id (dao-id uint))
  (let ((current-id (get-next-proposal-id dao-id)))
    (map-set next-proposal-id-by-dao dao-id (+ current-id u1))
    current-id
  )
)

;; Check if principal is a member of the DAO
(define-private (is-member (dao-id uint) (user principal))
  (is-some (map-get? dao-members { dao-id: dao-id, member: user }))
)

;; Check if principal is admin of the DAO
(define-private (is-admin (dao-id uint) (user principal))
  (match (map-get? dao-members { dao-id: dao-id, member: user })
    member (is-eq (get role member) "admin")
    false
  )
)

;; Check if DAO exists
(define-private (dao-exists (dao-id uint))
  (is-some (map-get? daos { dao-id: dao-id }))
)

;; Calculate if a proposal has reached the required threshold to pass
(define-private (proposal-reached-threshold (dao-id uint) (proposal-id uint))
  (match (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })
    proposal 
    (match (map-get? daos { dao-id: dao-id })
      dao
      (let ((total-votes (+ (get votes-for proposal) (get votes-against proposal)))
            (threshold (get voting-threshold dao))
            (for-percentage (* u100 (get votes-for proposal) (/ total-votes))))
        (>= for-percentage threshold))
      false)
    false)
)

;; Read-only functions

;; Retrieve DAO details
(define-read-only (get-dao-info (dao-id uint))
  (match (map-get? daos { dao-id: dao-id })
    dao (ok dao)
    ERR-DAO-NOT-FOUND
  )
)

;; Get member info for a DAO
(define-read-only (get-member-info (dao-id uint) (member principal))
  (match (map-get? dao-members { dao-id: dao-id, member: member })
    member-info (ok member-info)
    ERR-MEMBER-NOT-FOUND
  )
)

;; Get proposal info
(define-read-only (get-proposal-info (dao-id uint) (proposal-id uint))
  (match (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })
    proposal (ok proposal)
    ERR-PROPOSAL-NOT-FOUND
  )
)

;; Check if user has voted on a proposal
(define-read-only (has-voted (dao-id uint) (proposal-id uint) (voter principal))
  (is-some (map-get? votes { dao-id: dao-id, proposal-id: proposal-id, voter: voter }))
)

;; Public functions

;; Create a new DAO
(define-public (create-dao 
  (name (string-ascii 64)) 
  (description (string-utf8 256))
  (voting-threshold uint)
  (voting-period uint)
  (membership-type (string-ascii 10)))
  
  (let ((dao-id (var-get next-dao-id))
        (caller tx-sender)
        (block-height block-height))
    
    ;; Input validation
    (asserts! (and (>= voting-threshold u1) (<= voting-threshold u100)) ERR-INVALID-PARAMETER)
    (asserts! (> voting-period u0) ERR-INVALID-PARAMETER)
    (asserts! (or (is-eq membership-type "open") 
                  (is-eq membership-type "invite") 
                  (is-eq membership-type "token")) 
              ERR-INVALID-PARAMETER)
    
    ;; Create the DAO
    (map-set daos
      { dao-id: dao-id }
      { 
        name: name,
        description: description,
        creator: caller,
        created-at: block-height,
        voting-threshold: voting-threshold,
        voting-period: voting-period,
        membership-type: membership-type,
        member-count: u1
      }
    )
    
    ;; Add creator as admin member
    (map-set dao-members
      { dao-id: dao-id, member: caller }
      {
        joined-at: block-height,
        voting-power: u100,
        role: "admin"
      }
    )
    
    ;; Update the next DAO ID
    (var-set next-dao-id (+ dao-id u1))
    
    (ok dao-id)
  )
)

;; Join an open DAO
(define-public (join-dao (dao-id uint))
  (let ((caller tx-sender)
        (block-height block-height))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is not already a member
    (asserts! (not (is-member dao-id caller)) ERR-ALREADY-MEMBER)
    
    ;; Check that the DAO is open for membership
    (match (map-get? daos { dao-id: dao-id })
      dao (asserts! (is-eq (get membership-type dao) "open") ERR-NOT-AUTHORIZED)
      (err ERR-DAO-NOT-FOUND)
    )
    
    ;; Add the new member
    (map-set dao-members
      { dao-id: dao-id, member: caller }
      {
        joined-at: block-height,
        voting-power: u1,
        role: "member"
      }
    )
    
    ;; Update member count
    (match (map-get? daos { dao-id: dao-id })
      dao 
      (map-set daos
        { dao-id: dao-id }
        (merge dao { member-count: (+ (get member-count dao) u1) })
      )
      (err ERR-DAO-NOT-FOUND)
    )
    
    (ok true)
  )
)

;; Invite a member to a DAO (admin only)
(define-public (invite-member (dao-id uint) (new-member principal) (role (string-ascii 16)) (voting-power uint))
  (let ((caller tx-sender)
        (block-height block-height))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check authorization (only admins can invite)
    (asserts! (is-admin dao-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check that new-member is not already a member
    (asserts! (not (is-member dao-id new-member)) ERR-ALREADY-MEMBER)
    
    ;; Add the new member
    (map-set dao-members
      { dao-id: dao-id, member: new-member }
      {
        joined-at: block-height,
        voting-power: voting-power,
        role: role
      }
    )
    
    ;; Update member count
    (match (map-get? daos { dao-id: dao-id })
      dao 
      (map-set daos
        { dao-id: dao-id }
        (merge dao { member-count: (+ (get member-count dao) u1) })
      )
      (err ERR-DAO-NOT-FOUND)
    )
    
    (ok true)
  )
)

;; Remove a member from a DAO (admin only or self)
(define-public (remove-member (dao-id uint) (member-to-remove principal))
  (let ((caller tx-sender))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that member exists
    (asserts! (is-member dao-id member-to-remove) ERR-MEMBER-NOT-FOUND)
    
    ;; Check authorization (admin or self)
    (asserts! (or (is-admin dao-id caller) (is-eq caller member-to-remove)) ERR-NOT-AUTHORIZED)
    
    ;; Remove the member
    (map-delete dao-members { dao-id: dao-id, member: member-to-remove })
    
    ;; Update member count
    (match (map-get? daos { dao-id: dao-id })
      dao 
      (map-set daos
        { dao-id: dao-id }
        (merge dao { member-count: (- (get member-count dao) u1) })
      )
      (err ERR-DAO-NOT-FOUND)
    )
    
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (dao-id uint) 
  (title (string-ascii 64)) 
  (description (string-utf8 256)))
  
  (let ((caller tx-sender)
        (block-height block-height)
        (proposal-id (update-next-proposal-id dao-id)))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is a member
    (asserts! (is-member dao-id caller) ERR-NOT-MEMBER)
    
    ;; Get voting period
    (match (map-get? daos { dao-id: dao-id })
      dao
      (let ((voting-period (get voting-period dao))
            (expires-at (+ block-height voting-period)))
        
        ;; Create the proposal
        (map-set proposals
          { dao-id: dao-id, proposal-id: proposal-id }
          {
            title: title,
            description: description,
            proposer: caller,
            created-at: block-height,
            expires-at: expires-at,
            executed: false,
            votes-for: u0,
            votes-against: u0,
            status: "active"
          }
        )
        
        (ok proposal-id))
      (err ERR-DAO-NOT-FOUND)
    )
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (dao-id uint) (proposal-id uint) (vote-for bool))
  (let ((caller tx-sender)
        (block-height block-height))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is a member
    (asserts! (is-member dao-id caller) ERR-NOT-MEMBER)
    
    ;; Check that proposal exists
    (asserts! (is-some (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })) ERR-PROPOSAL-NOT-FOUND)
    
    ;; Check that member hasn't voted already
    (asserts! (not (has-voted dao-id proposal-id caller)) ERR-ALREADY-VOTED)
    
    ;; Check that proposal is still active
    (match (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })
      proposal
      (begin
        (asserts! (< block-height (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-ACTIVE)
        
        ;; Get member's voting power
        (match (map-get? dao-members { dao-id: dao-id, member: caller })
          member
          (let ((voting-power (get voting-power member)))
            
            ;; Record the vote
            (map-set votes
              { dao-id: dao-id, proposal-id: proposal-id, voter: caller }
              {
                vote: vote-for,
                weight: voting-power,
                voted-at: block-height
              }
            )
            
            ;; Update proposal vote counts
            (if vote-for
              (map-set proposals
                { dao-id: dao-id, proposal-id: proposal-id }
                (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) })
              )
              (map-set proposals
                { dao-id: dao-id, proposal-id: proposal-id }
                (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) })
              )
            )
            
            (ok true))
          (err ERR-MEMBER-NOT-FOUND)
        ))
      (err ERR-PROPOSAL-NOT-FOUND)
    )
  )
)

;; Finalize a proposal after voting period
(define-public (finalize-proposal (dao-id uint) (proposal-id uint))
  (let ((block-height block-height))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that proposal exists and is ready to be finalized
    (match (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })
      proposal
      (begin
        (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-ACTIVE)
        (asserts! (>= block-height (get expires-at proposal)) ERR-PROPOSAL-ACTIVE)
        
        ;; Determine if proposal passed
        (let ((passed (proposal-reached-threshold dao-id proposal-id)))
          
          ;; Update proposal status
          (map-set proposals
            { dao-id: dao-id, proposal-id: proposal-id }
            (merge proposal 
              { 
                status: (if passed "passed" "rejected"),
                executed: false
              }
            )
          )
          
          (ok passed))
      )
      (err ERR-PROPOSAL-NOT-FOUND)
    )
  )
)

;; Execute a passed proposal (marking it as executed)
(define-public (execute-proposal (dao-id uint) (proposal-id uint))
  (let ((caller tx-sender))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is an admin
    (asserts! (is-admin dao-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check that proposal exists and can be executed
    (match (map-get? proposals { dao-id: dao-id, proposal-id: proposal-id })
      proposal
      (begin
        (asserts! (is-eq (get status proposal) "passed") ERR-INSUFFICIENT-VOTES)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-ACTIVE)
        
        ;; Mark as executed
        (map-set proposals
          { dao-id: dao-id, proposal-id: proposal-id }
          (merge proposal { executed: true })
        )
        
        (ok true))
      (err ERR-PROPOSAL-NOT-FOUND)
    )
  )
)

;; Update DAO settings (admin only)
(define-public (update-dao-settings 
  (dao-id uint)
  (name (string-ascii 64))
  (description (string-utf8 256))
  (voting-threshold uint)
  (voting-period uint)
  (membership-type (string-ascii 10)))
  
  (let ((caller tx-sender))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is an admin
    (asserts! (is-admin dao-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Input validation
    (asserts! (and (>= voting-threshold u1) (<= voting-threshold u100)) ERR-INVALID-PARAMETER)
    (asserts! (> voting-period u0) ERR-INVALID-PARAMETER)
    (asserts! (or (is-eq membership-type "open") 
                  (is-eq membership-type "invite") 
                  (is-eq membership-type "token")) 
              ERR-INVALID-PARAMETER)
    
    ;; Update DAO settings
    (match (map-get? daos { dao-id: dao-id })
      dao
      (map-set daos
        { dao-id: dao-id }
        (merge dao 
          {
            name: name,
            description: description,
            voting-threshold: voting-threshold,
            voting-period: voting-period,
            membership-type: membership-type
          }
        )
      )
      (err ERR-DAO-NOT-FOUND)
    )
    
    (ok true)
  )
)

;; Update member role and voting power (admin only)
(define-public (update-member (dao-id uint) (member-to-update principal) (role (string-ascii 16)) (voting-power uint))
  (let ((caller tx-sender))
    
    ;; Check that DAO exists
    (asserts! (dao-exists dao-id) ERR-DAO-NOT-FOUND)
    
    ;; Check that caller is an admin
    (asserts! (is-admin dao-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Check that member exists
    (asserts! (is-member dao-id member-to-update) ERR-MEMBER-NOT-FOUND)
    
    ;; Update member
    (match (map-get? dao-members { dao-id: dao-id, member: member-to-update })
      member
      (map-set dao-members
        { dao-id: dao-id, member: member-to-update }
        (merge member 
          {
            voting-power: voting-power,
            role: role
          }
        )
      )
      (err ERR-MEMBER-NOT-FOUND)
    )
    
    (ok true)
  )
)