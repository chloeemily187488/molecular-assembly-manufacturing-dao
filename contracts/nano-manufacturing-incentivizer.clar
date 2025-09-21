;; Nano-Manufacturing Incentivizer Contract
;; Coordinates distributed molecular manufacturing tasks across decentralized fabrication nodes,
;; manages quality control for nano-scale production, and distributes rewards to manufacturers
;; based on atomic precision and successful molecular assembly completion.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_TASK_NOT_FOUND (err u201))
(define-constant ERR_TASK_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_TASK_DATA (err u203))
(define-constant ERR_INSUFFICIENT_STAKE (err u204))
(define-constant ERR_TASK_NOT_ACTIVE (err u205))
(define-constant ERR_ALREADY_CLAIMED (err u206))
(define-constant ERR_QUALITY_CHECK_FAILED (err u207))
(define-constant ERR_NODE_NOT_REGISTERED (err u208))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u209))
(define-constant ERR_TASK_EXPIRED (err u210))
(define-constant ERR_INVALID_SUBMISSION (err u211))

;; Data Variables
(define-data-var task-counter uint u0)
(define-data-var submission-counter uint u0)
(define-data-var node-counter uint u0)
(define-data-var base-reward uint u10000000) ;; 10 STX in microSTX
(define-data-var quality-threshold uint u85) ;; 85% quality threshold
(define-data-var min-reputation uint u50) ;; Minimum reputation score
(define-data-var stake-requirement uint u1000000) ;; 1 STX stake requirement

;; Manufacturing Task Structure
(define-map manufacturing-tasks
  uint
  {
    task-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    blueprint-id: uint,
    requester: principal,
    target-quantity: uint,
    precision-requirement: uint, ;; 1-100 scale (atomic precision level)
    complexity-level: uint, ;; 1-10 scale
    reward-amount: uint,
    creation-timestamp: uint,
    deadline: uint,
    status: uint, ;; 0=open, 1=assigned, 2=in-progress, 3=submitted, 4=completed, 5=cancelled
    assigned-node: (optional principal),
    required-equipment: (list 10 (string-ascii 50)),
    safety-requirements: (list 5 (string-ascii 100)),
    quality-metrics: (list 5 (string-ascii 50))
  }
)

;; Manufacturing Node Registration
(define-map manufacturing-nodes
  principal
  {
    node-id: uint,
    operator: principal,
    node-name: (string-ascii 100),
    location: (string-ascii 100),
    equipment-list: (list 20 (string-ascii 50)),
    specializations: (list 10 (string-ascii 50)),
    precision-capability: uint, ;; Max precision level (1-100)
    capacity-rating: uint, ;; Concurrent tasks (1-10)
    reputation-score: uint, ;; 0-100 scale
    total-completed-tasks: uint,
    success-rate: uint, ;; 0-100 percentage
    registration-timestamp: uint,
    stake-amount: uint,
    is-active: bool,
    certification-level: uint ;; 1-5 certification level
  }
)

;; Task Submissions
(define-map task-submissions
  uint
  {
    submission-id: uint,
    task-id: uint,
    manufacturer: principal,
    submission-timestamp: uint,
    production-data: (buff 512),
    quality-metrics: (list 5 uint),
    precision-achieved: uint,
    quantity-produced: uint,
    manufacturing-duration: uint,
    energy-consumption: uint,
    material-efficiency: uint,
    validation-status: uint, ;; 0=pending, 1=passed, 2=failed
    validator: (optional principal),
    validation-notes: (string-ascii 300)
  }
)

;; Quality Control Validators
(define-map quality-validators
  principal
  {
    validator-name: (string-ascii 100),
    expertise-areas: (list 10 (string-ascii 50)),
    certification-level: uint,
    validation-count: uint,
    accuracy-score: uint, ;; 0-100 scale
    registration-timestamp: uint,
    is-authorized: bool
  }
)

;; Reward Distribution Records
(define-map reward-distributions
  {task-id: uint, recipient: principal}
  {
    amount: uint,
    distribution-timestamp: uint,
    bonus-multiplier: uint, ;; 100 = 1x, 150 = 1.5x, etc.
    performance-score: uint,
    distribution-reason: (string-ascii 100)
  }
)

;; Node Performance Analytics
(define-map node-performance
  {node: principal, period: uint}
  {
    tasks-completed: uint,
    average-quality: uint,
    average-precision: uint,
    efficiency-score: uint,
    reliability-score: uint,
    total-earnings: uint
  }
)

;; Task Assignment Queue
(define-map task-assignments
  {task-id: uint, node: principal}
  {
    assignment-timestamp: uint,
    estimated-completion: uint,
    stake-amount: uint,
    commitment-level: uint
  }
)

;; Helper Functions

;; Generate unique task ID
(define-private (get-next-task-id)
  (begin
    (var-set task-counter (+ (var-get task-counter) u1))
    (var-get task-counter)
  )
)

;; Generate unique submission ID
(define-private (get-next-submission-id)
  (begin
    (var-set submission-counter (+ (var-get submission-counter) u1))
    (var-get submission-counter)
  )
)

;; Generate unique node ID
(define-private (get-next-node-id)
  (begin
    (var-set node-counter (+ (var-get node-counter) u1))
    (var-get node-counter)
  )
)

;; Validate task data
(define-private (validate-task-data
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-quantity uint)
  (precision-requirement uint)
  (complexity-level uint)
  (reward-amount uint))
  (and
    (> (len title) u0)
    (> (len description) u10)
    (> target-quantity u0)
    (<= precision-requirement u100)
    (>= precision-requirement u1)
    (<= complexity-level u10)
    (>= complexity-level u1)
    (> reward-amount u0)
  )
)

;; Calculate reward bonus based on performance
(define-private (calculate-bonus-multiplier (precision uint) (quality uint) (efficiency uint))
  (let (
    (precision-bonus (if (>= precision u95) u25 (if (>= precision u90) u15 (if (>= precision u80) u5 u0))))
    (quality-bonus (if (>= quality u95) u20 (if (>= quality u90) u10 (if (>= quality u80) u5 u0))))
    (efficiency-bonus (if (>= efficiency u95) u15 (if (>= efficiency u85) u5 u0)))
  )
    (+ u100 precision-bonus quality-bonus efficiency-bonus)
  )
)

;; Check if node meets task requirements
(define-private (node-meets-requirements (node principal) (task-id uint))
  (match (map-get? manufacturing-nodes node)
    node-data
      (match (map-get? manufacturing-tasks task-id)
        task-data
          (and
            (get is-active node-data)
            (>= (get precision-capability node-data) (get precision-requirement task-data))
            (>= (get reputation-score node-data) (var-get min-reputation))
            (>= (get stake-amount node-data) (var-get stake-requirement))
          )
        false
      )
    false
  )
)

;; Public Functions

;; Register as a manufacturing node
(define-public (register-manufacturing-node
  (node-name (string-ascii 100))
  (location (string-ascii 100))
  (equipment-list (list 20 (string-ascii 50)))
  (specializations (list 10 (string-ascii 50)))
  (precision-capability uint)
  (capacity-rating uint)
  (certification-level uint))
  (let (
    (node-id (get-next-node-id))
    (stake-amount (var-get stake-requirement))
  )
    (asserts! (> (len node-name) u0) ERR_INVALID_TASK_DATA)
    (asserts! (<= precision-capability u100) ERR_INVALID_TASK_DATA)
    (asserts! (<= capacity-rating u10) ERR_INVALID_TASK_DATA)
    (asserts! (<= certification-level u5) ERR_INVALID_TASK_DATA)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_STAKE)
    
    ;; Transfer stake amount
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Register node
    (map-set manufacturing-nodes tx-sender {
      node-id: node-id,
      operator: tx-sender,
      node-name: node-name,
      location: location,
      equipment-list: equipment-list,
      specializations: specializations,
      precision-capability: precision-capability,
      capacity-rating: capacity-rating,
      reputation-score: u50, ;; Starting reputation
      total-completed-tasks: u0,
      success-rate: u100,
      registration-timestamp: stacks-block-height,
      stake-amount: stake-amount,
      is-active: true,
      certification-level: certification-level
    })
    
    (ok node-id)
  )
)

;; Create a new manufacturing task
(define-public (create-manufacturing-task
  (title (string-ascii 100))
  (description (string-ascii 500))
  (blueprint-id uint)
  (target-quantity uint)
  (precision-requirement uint)
  (complexity-level uint)
  (reward-amount uint)
  (deadline-blocks uint)
  (required-equipment (list 10 (string-ascii 50)))
  (safety-requirements (list 5 (string-ascii 100)))
  (quality-metrics (list 5 (string-ascii 50))))
  (let ((task-id (get-next-task-id)))
    (asserts! (validate-task-data title description target-quantity precision-requirement complexity-level reward-amount) ERR_INVALID_TASK_DATA)
    (asserts! (>= (stx-get-balance tx-sender) reward-amount) ERR_INSUFFICIENT_STAKE)
    
    ;; Escrow reward amount
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    
    ;; Create task
    (map-set manufacturing-tasks task-id {
      task-id: task-id,
      title: title,
      description: description,
      blueprint-id: blueprint-id,
      requester: tx-sender,
      target-quantity: target-quantity,
      precision-requirement: precision-requirement,
      complexity-level: complexity-level,
      reward-amount: reward-amount,
      creation-timestamp: stacks-block-height,
      deadline: (+ stacks-block-height deadline-blocks),
      status: u0, ;; Open
      assigned-node: none,
      required-equipment: required-equipment,
      safety-requirements: safety-requirements,
      quality-metrics: quality-metrics
    })
    
    (ok task-id)
  )
)

;; Assign task to manufacturing node
(define-public (assign-task (task-id uint) (node principal))
  (let ((task (unwrap! (map-get? manufacturing-tasks task-id) ERR_TASK_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get requester task)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status task) u0) ERR_TASK_NOT_ACTIVE)
    (asserts! (node-meets-requirements node task-id) ERR_NODE_NOT_REGISTERED)
    
    ;; Update task status
    (map-set manufacturing-tasks task-id
      (merge task {
        status: u1, ;; Assigned
        assigned-node: (some node)
      })
    )
    
    ;; Create assignment record
    (map-set task-assignments
      {task-id: task-id, node: node}
      {
        assignment-timestamp: stacks-block-height,
        estimated-completion: (+ stacks-block-height u1440), ;; ~1 day
        stake-amount: (var-get stake-requirement),
        commitment-level: u100
      }
    )
    
    (ok true)
  )
)

;; Submit manufacturing results
(define-public (submit-manufacturing-result
  (task-id uint)
  (production-data (buff 512))
  (quality-metrics (list 5 uint))
  (precision-achieved uint)
  (quantity-produced uint)
  (manufacturing-duration uint)
  (energy-consumption uint)
  (material-efficiency uint))
  (let (
    (task (unwrap! (map-get? manufacturing-tasks task-id) ERR_TASK_NOT_FOUND))
    (submission-id (get-next-submission-id))
  )
    (asserts! (is-eq (some tx-sender) (get assigned-node task)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status task) u1) ERR_TASK_NOT_ACTIVE)
    (asserts! (<= precision-achieved u100) ERR_INVALID_SUBMISSION)
    
    ;; Create submission
    (map-set task-submissions submission-id {
      submission-id: submission-id,
      task-id: task-id,
      manufacturer: tx-sender,
      submission-timestamp: stacks-block-height,
      production-data: production-data,
      quality-metrics: quality-metrics,
      precision-achieved: precision-achieved,
      quantity-produced: quantity-produced,
      manufacturing-duration: manufacturing-duration,
      energy-consumption: energy-consumption,
      material-efficiency: material-efficiency,
      validation-status: u0, ;; Pending
      validator: none,
      validation-notes: ""
    })
    
    ;; Update task status
    (map-set manufacturing-tasks task-id
      (merge task {
        status: u3 ;; Submitted
      })
    )
    
    (ok submission-id)
  )
)

;; Validate manufacturing quality
(define-public (validate-quality
  (submission-id uint)
  (validation-passed bool)
  (validation-notes (string-ascii 300)))
  (let ((submission (unwrap! (map-get? task-submissions submission-id) ERR_INVALID_SUBMISSION)))
    ;; For simplicity, allow anyone to validate (in production, should be authorized validators)
    (asserts! (is-eq (get validation-status submission) u0) ERR_ALREADY_CLAIMED)
    
    ;; Update submission
    (map-set task-submissions submission-id
      (merge submission {
        validation-status: (if validation-passed u1 u2),
        validator: (some tx-sender),
        validation-notes: validation-notes
      })
    )
    
    ;; If validation passed, distribute rewards
    (if validation-passed
      (begin
        (let (
          (task-id (get task-id submission))
          (task (unwrap-panic (map-get? manufacturing-tasks task-id)))
          (manufacturer (get manufacturer submission))
          (precision (get precision-achieved submission))
          (avg-quality (fold + (get quality-metrics submission) u0))
          (efficiency (get material-efficiency submission))
          (bonus-multiplier (calculate-bonus-multiplier precision avg-quality efficiency))
          (task-reward (get reward-amount task))
          (final-reward (/ (* task-reward bonus-multiplier) u100))
        )
          ;; Transfer reward to manufacturer
          (try! (as-contract (stx-transfer? final-reward tx-sender manufacturer)))
          
          ;; Update task status to completed
          (map-set manufacturing-tasks task-id
            (merge task {
              status: u4 ;; Completed
            })
          )
          
          ;; Record reward distribution
          (map-set reward-distributions
            {task-id: task-id, recipient: manufacturer}
            {
              amount: final-reward,
              distribution-timestamp: stacks-block-height,
              bonus-multiplier: bonus-multiplier,
              performance-score: avg-quality,
              distribution-reason: "Task completion reward"
            }
          )
          
          ;; Update node performance
          (match (map-get? manufacturing-nodes manufacturer)
            node-data
              (map-set manufacturing-nodes manufacturer
                (merge node-data {
                  total-completed-tasks: (+ (get total-completed-tasks node-data) u1),
                  reputation-score: (if (> (+ (get reputation-score node-data) u2) u100) u100 (+ (get reputation-score node-data) u2))
                })
              )
            false ;; Node not found, should not happen
          )
        )
        (ok true)
      )
      (ok false)
    )
  )
)

;; Read-only Functions

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? manufacturing-tasks task-id)
)

;; Get manufacturing node details
(define-read-only (get-manufacturing-node (node principal))
  (map-get? manufacturing-nodes node)
)

;; Get submission details
(define-read-only (get-submission (submission-id uint))
  (map-get? task-submissions submission-id)
)

;; Get reward distribution record
(define-read-only (get-reward-distribution (task-id uint) (recipient principal))
  (map-get? reward-distributions {task-id: task-id, recipient: recipient})
)

;; Get task assignment
(define-read-only (get-task-assignment (task-id uint) (node principal))
  (map-get? task-assignments {task-id: task-id, node: node})
)

;; Get node performance for period
(define-read-only (get-node-performance (node principal) (period uint))
  (map-get? node-performance {node: node, period: period})
)

;; Get current task counter
(define-read-only (get-task-counter)
  (var-get task-counter)
)

;; Get current submission counter
(define-read-only (get-submission-counter)
  (var-get submission-counter)
)

;; Get current node counter
(define-read-only (get-node-counter)
  (var-get node-counter)
)

;; Get base reward amount
(define-read-only (get-base-reward)
  (var-get base-reward)
)

;; Get quality threshold
(define-read-only (get-quality-threshold)
  (var-get quality-threshold)
)

;; Get minimum reputation requirement
(define-read-only (get-min-reputation)
  (var-get min-reputation)
)

;; Get stake requirement
(define-read-only (get-stake-requirement)
  (var-get stake-requirement)
)


;; title: nano-manufacturing-incentivizer
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

