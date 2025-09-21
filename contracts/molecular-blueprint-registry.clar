;; Molecular Blueprint Registry Contract
;; Stores and validates molecular assembly instructions with cryptographic integrity verification,
;; manages intellectual property rights for atomic-scale designs, and provides secure distribution
;; of nanotechnology manufacturing protocols across the network.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_BLUEPRINT_NOT_FOUND (err u101))
(define-constant ERR_BLUEPRINT_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_BLUEPRINT_DATA (err u103))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u104))
(define-constant ERR_LICENSE_NOT_FOUND (err u105))
(define-constant ERR_ALREADY_LICENSED (err u106))
(define-constant ERR_ACCESS_DENIED (err u107))
(define-constant ERR_INVALID_VERSION (err u108))

;; Data Variables
(define-data-var blueprint-counter uint u0)
(define-data-var license-counter uint u0)
(define-data-var registry-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var licensing-fee uint u500000) ;; 0.5 STX in microSTX

;; Blueprint Structure
(define-map blueprints
  uint
  {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    molecular-formula: (string-ascii 200),
    complexity-level: uint, ;; 1-10 scale
    blueprint-hash: (buff 32),
    metadata-hash: (buff 32),
    creation-timestamp: uint,
    version: uint,
    is-active: bool,
    access-level: uint, ;; 0=public, 1=licensed, 2=private
    manufacturing-cost: uint,
    safety-rating: uint, ;; 1-10 scale
    quality-score: uint ;; 0-100 scale
  }
)

;; Blueprint Versions (for version control)
(define-map blueprint-versions
  {blueprint-id: uint, version: uint}
  {
    blueprint-hash: (buff 32),
    metadata-hash: (buff 32),
    change-description: (string-ascii 300),
    timestamp: uint,
    updated-by: principal
  }
)

;; IP Rights and Licensing
(define-map licenses
  uint
  {
    license-id: uint,
    blueprint-id: uint,
    licensee: principal,
    licensor: principal,
    license-type: uint, ;; 0=view, 1=manufacture, 2=modify, 3=distribute
    granted-timestamp: uint,
    expiry-timestamp: uint,
    royalty-percentage: uint,
    is-active: bool,
    usage-count: uint,
    max-usage: uint
  }
)

;; Access Control Lists
(define-map blueprint-permissions
  {blueprint-id: uint, user: principal}
  {
    permission-level: uint, ;; 0=none, 1=view, 2=edit, 3=admin
    granted-by: principal,
    granted-timestamp: uint
  }
)

;; Blueprint Categories and Tags
(define-map blueprint-categories
  uint
  (list 10 (string-ascii 50))
)

;; Quality Validation Records
(define-map quality-validations
  {blueprint-id: uint, validator: principal}
  {
    validation-score: uint,
    validation-timestamp: uint,
    validation-notes: (string-ascii 300),
    is-certified: bool
  }
)

;; Blueprint Discovery Index
(define-map creator-blueprints
  principal
  (list 100 uint)
)

;; Revenue Tracking
(define-map creator-revenue
  principal
  {
    total-earned: uint,
    total-royalties: uint,
    active-licenses: uint
  }
)

;; Helper Functions

;; Generate unique blueprint ID
(define-private (get-next-blueprint-id)
  (begin
    (var-set blueprint-counter (+ (var-get blueprint-counter) u1))
    (var-get blueprint-counter)
  )
)

;; Generate unique license ID
(define-private (get-next-license-id)
  (begin
    (var-set license-counter (+ (var-get license-counter) u1))
    (var-get license-counter)
  )
)

;; Validate blueprint data
(define-private (validate-blueprint-data 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (molecular-formula (string-ascii 200))
  (complexity-level uint)
  (safety-rating uint))
  (and
    (> (len title) u0)
    (> (len description) u10)
    (> (len molecular-formula) u0)
    (<= complexity-level u10)
    (>= complexity-level u1)
    (<= safety-rating u10)
    (>= safety-rating u1)
  )
)

;; Check if user has permission for blueprint
(define-private (has-permission (blueprint-id uint) (user principal) (required-level uint))
  (let ((permission (map-get? blueprint-permissions {blueprint-id: blueprint-id, user: user})))
    (match permission
      perm (>= (get permission-level perm) required-level)
      false
    )
  )
)

;; Public Functions

;; Register a new molecular blueprint
(define-public (register-blueprint
  (title (string-ascii 100))
  (description (string-ascii 500))
  (molecular-formula (string-ascii 200))
  (complexity-level uint)
  (blueprint-hash (buff 32))
  (metadata-hash (buff 32))
  (access-level uint)
  (manufacturing-cost uint)
  (safety-rating uint)
  (categories (list 10 (string-ascii 50))))
  (let (
    (blueprint-id (get-next-blueprint-id))
    (payment-amount (var-get registry-fee))
  )
    (asserts! (validate-blueprint-data title description molecular-formula complexity-level safety-rating) ERR_INVALID_BLUEPRINT_DATA)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer registration fee
    (try! (stx-transfer? payment-amount tx-sender CONTRACT_OWNER))
    
    ;; Store blueprint
    (map-set blueprints blueprint-id {
      id: blueprint-id,
      title: title,
      description: description,
      creator: tx-sender,
      molecular-formula: molecular-formula,
      complexity-level: complexity-level,
      blueprint-hash: blueprint-hash,
      metadata-hash: metadata-hash,
      creation-timestamp: stacks-block-height,
      version: u1,
      is-active: true,
      access-level: access-level,
      manufacturing-cost: manufacturing-cost,
      safety-rating: safety-rating,
      quality-score: u0
    })
    
    ;; Store initial version
    (map-set blueprint-versions 
      {blueprint-id: blueprint-id, version: u1}
      {
        blueprint-hash: blueprint-hash,
        metadata-hash: metadata-hash,
        change-description: "Initial version",
        timestamp: stacks-block-height,
        updated-by: tx-sender
      }
    )
    
    ;; Set creator permissions
    (map-set blueprint-permissions
      {blueprint-id: blueprint-id, user: tx-sender}
      {
        permission-level: u3,
        granted-by: tx-sender,
        granted-timestamp: stacks-block-height
      }
    )
    
    ;; Store categories
    (map-set blueprint-categories blueprint-id categories)
    
    ;; Update creator's blueprint list
    (let ((current-list (default-to (list) (map-get? creator-blueprints tx-sender))))
      (map-set creator-blueprints tx-sender (unwrap-panic (as-max-len? (append current-list blueprint-id) u100)))
    )
    
    (ok blueprint-id)
  )
)

;; Update an existing blueprint (create new version)
(define-public (update-blueprint
  (blueprint-id uint)
  (new-blueprint-hash (buff 32))
  (new-metadata-hash (buff 32))
  (change-description (string-ascii 300)))
  (let ((blueprint (unwrap! (map-get? blueprints blueprint-id) ERR_BLUEPRINT_NOT_FOUND)))
    (asserts! (has-permission blueprint-id tx-sender u2) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active blueprint) ERR_ACCESS_DENIED)
    
    (let ((new-version (+ (get version blueprint) u1)))
      ;; Store new version
      (map-set blueprint-versions
        {blueprint-id: blueprint-id, version: new-version}
        {
          blueprint-hash: new-blueprint-hash,
          metadata-hash: new-metadata-hash,
          change-description: change-description,
          timestamp: stacks-block-height,
          updated-by: tx-sender
        }
      )
      
      ;; Update main blueprint record
      (map-set blueprints blueprint-id
        (merge blueprint {
          blueprint-hash: new-blueprint-hash,
          metadata-hash: new-metadata-hash,
          version: new-version
        })
      )
      
      (ok new-version)
    )
  )
)

;; Grant license for blueprint usage
(define-public (grant-license
  (blueprint-id uint)
  (licensee principal)
  (license-type uint)
  (duration-blocks uint)
  (royalty-percentage uint)
  (max-usage uint))
  (let (
    (blueprint (unwrap! (map-get? blueprints blueprint-id) ERR_BLUEPRINT_NOT_FOUND))
    (license-id (get-next-license-id))
    (payment-amount (var-get licensing-fee))
  )
    (asserts! (is-eq tx-sender (get creator blueprint)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active blueprint) ERR_ACCESS_DENIED)
    (asserts! (>= (stx-get-balance licensee) payment-amount) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer licensing fee from licensee
    (try! (stx-transfer? payment-amount licensee tx-sender))
    
    ;; Create license
    (map-set licenses license-id {
      license-id: license-id,
      blueprint-id: blueprint-id,
      licensee: licensee,
      licensor: tx-sender,
      license-type: license-type,
      granted-timestamp: stacks-block-height,
      expiry-timestamp: (+ stacks-block-height duration-blocks),
      royalty-percentage: royalty-percentage,
      is-active: true,
      usage-count: u0,
      max-usage: max-usage
    })
    
    ;; Grant access permission
    (map-set blueprint-permissions
      {blueprint-id: blueprint-id, user: licensee}
      {
        permission-level: u1,
        granted-by: tx-sender,
        granted-timestamp: stacks-block-height
      }
    )
    
    (ok license-id)
  )
)

;; Submit quality validation
(define-public (submit-quality-validation
  (blueprint-id uint)
  (validation-score uint)
  (validation-notes (string-ascii 300))
  (is-certified bool))
  (let ((blueprint (unwrap! (map-get? blueprints blueprint-id) ERR_BLUEPRINT_NOT_FOUND)))
    (asserts! (has-permission blueprint-id tx-sender u1) ERR_ACCESS_DENIED)
    (asserts! (<= validation-score u100) ERR_INVALID_BLUEPRINT_DATA)
    
    ;; Store validation
    (map-set quality-validations
      {blueprint-id: blueprint-id, validator: tx-sender}
      {
        validation-score: validation-score,
        validation-timestamp: stacks-block-height,
        validation-notes: validation-notes,
        is-certified: is-certified
      }
    )
    
    ;; Update blueprint quality score (simple average for now)
    (let ((current-score (get quality-score blueprint)))
      (map-set blueprints blueprint-id
        (merge blueprint {
          quality-score: (/ (+ current-score validation-score) u2)
        })
      )
    )
    
    (ok true)
  )
)

;; Read-only Functions

;; Get blueprint details
(define-read-only (get-blueprint (blueprint-id uint))
  (map-get? blueprints blueprint-id)
)

;; Get blueprint version
(define-read-only (get-blueprint-version (blueprint-id uint) (version uint))
  (map-get? blueprint-versions {blueprint-id: blueprint-id, version: version})
)

;; Get license details
(define-read-only (get-license (license-id uint))
  (map-get? licenses license-id)
)

;; Get user permission level
(define-read-only (get-permission-level (blueprint-id uint) (user principal))
  (match (map-get? blueprint-permissions {blueprint-id: blueprint-id, user: user})
    perm (some (get permission-level perm))
    none
  )
)

;; Get creator's blueprints
(define-read-only (get-creator-blueprints (creator principal))
  (default-to (list) (map-get? creator-blueprints creator))
)

;; Get blueprint categories
(define-read-only (get-blueprint-categories (blueprint-id uint))
  (map-get? blueprint-categories blueprint-id)
)

;; Check if blueprint is accessible to user
(define-read-only (is-blueprint-accessible (blueprint-id uint) (user principal))
  (match (map-get? blueprints blueprint-id)
    blueprint
      (or
        (is-eq (get access-level blueprint) u0) ;; public access
        (has-permission blueprint-id user u1) ;; has permission
        (is-eq user (get creator blueprint)) ;; is creator
      )
    false
  )
)

;; Get quality validation
(define-read-only (get-quality-validation (blueprint-id uint) (validator principal))
  (map-get? quality-validations {blueprint-id: blueprint-id, validator: validator})
)

;; Get current blueprint counter
(define-read-only (get-blueprint-counter)
  (var-get blueprint-counter)
)

;; Get current license counter
(define-read-only (get-license-counter)
  (var-get license-counter)
)

;; Get registry fee
(define-read-only (get-registry-fee)
  (var-get registry-fee)
)

;; Get licensing fee
(define-read-only (get-licensing-fee)
  (var-get licensing-fee)
)


;; title: molecular-blueprint-registry
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

