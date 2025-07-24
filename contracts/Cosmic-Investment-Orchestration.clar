;; Cosmic Investment Orchestration Hub - Multi-Asset Treasury Distribution Engine
;; Advanced financial diversification platform for decentralized portfolio allocation strategies
;; 
;; Next-generation infrastructure for managing cross-asset treasury distributions and risk mitigation
;; Enterprise-grade security with immutable allocation tracking and comprehensive audit capabilities
;; Dynamic asset lifecycle management with sophisticated rebalancing controls and temporal constraints
;; Multi-dimensional diversification mechanisms with cryptographic validation and performance analytics

;; Treasury allocation error classification for comprehensive exception handling across operations
(define-constant asset-allocation-missing (err u301))
(define-constant rebalancing-in-progress (err u302))
(define-constant distribution-parameters-invalid (err u303))
(define-constant investment-window-closed (err u304))
(define-constant treasury-access-denied (err u305))
(define-constant allocation-authority-failed (err u306))
(define-constant treasury-admin-required (err u300))
(define-constant restricted-treasury-operation (err u307))
(define-constant unsupported-asset-type (err u308))

;; Global allocation tracking system for unique treasury distribution identification
(define-data-var master-allocation-counter uint u0)

;; Supreme treasury overseer with ultimate control over diversification operations
(define-constant primary-treasury-controller tx-sender)

;; Central treasury allocation registry with comprehensive distribution metadata
(define-map treasury-distribution-vault
  { allocation-id: uint }
  {
    distribution-label: (string-ascii 64),
    treasury-manager: principal,
    allocation-percentage: uint,
    distribution-genesis: uint,
    rebalancing-horizon: uint,
    investment-thesis: (string-ascii 128),
    target-asset-classes: (list 10 (string-ascii 32))
  }
)

;; Treasury access control framework for managing operational permissions
(define-map treasury-access-registry
  { allocation-id: uint, accessor-principal: principal }
  { permission-level: uint }
)

;; Asset performance tracking for comprehensive portfolio analytics
(define-map asset-performance-metrics
  { allocation-id: uint }
  {
    total-value-locked: uint,
    performance-score: uint,
    risk-assessment: uint,
    last-rebalance: uint
  }
)

;; ===== Core validation utilities and data integrity enforcement functions =====

;; Validates structural integrity and format requirements for asset class identifiers
(define-private (validate-asset-class-format (asset-class (string-ascii 32)))
  (and
    (> (len asset-class) u0)
    (< (len asset-class) u33)
  )
)

;; Comprehensive validation framework for asset class arrays with structural verification
(define-private (verify-asset-class-collection (asset-list (list 10 (string-ascii 32))))
  (and
    (> (len asset-list) u0)
    (<= (len asset-list) u10)
    (is-eq (len (filter validate-asset-class-format asset-list)) (len asset-list))
  )
)

;; Confirms treasury allocation existence within the distribution vault system
(define-private (validate-allocation-exists (allocation-id uint))
  (is-some (map-get? treasury-distribution-vault { allocation-id: allocation-id }))
)

;; Secure retrieval of allocation percentage with protective fallback mechanisms
(define-private (fetch-allocation-percentage (allocation-id uint))
  (default-to u0
    (get allocation-percentage
      (map-get? treasury-distribution-vault { allocation-id: allocation-id })
    )
  )
)

;; Multi-layer security validation for treasury management authority verification
(define-private (confirm-treasury-authority (allocation-id uint) (manager-address principal))
  (match (map-get? treasury-distribution-vault { allocation-id: allocation-id })
    allocation-data (is-eq (get treasury-manager allocation-data) manager-address)
    false
  )
)

;; Temporal validation ensuring rebalancing window remains active and valid
(define-private (check-rebalancing-window (allocation-id uint))
  (match (map-get? treasury-distribution-vault { allocation-id: allocation-id })
    allocation-data (< block-height (get rebalancing-horizon allocation-data))
    false
  )
)

;; Permission level validation for treasury operation authorization
(define-private (validate-permission-level (allocation-id uint) (accessor principal) (required-level uint))
  (let
    (
      (current-permission (default-to u0
        (get permission-level
          (map-get? treasury-access-registry { allocation-id: allocation-id, accessor-principal: accessor })
        )
      ))
    )
    (>= current-permission required-level)
  )
)

;; ===== Primary public interface functions for treasury diversification management =====

;; Creates comprehensive treasury allocation with multi-asset distribution configuration
(define-public (create-treasury-allocation
  (distribution-label (string-ascii 64))
  (allocation-percentage uint)
  (rebalancing-duration uint)
  (investment-thesis (string-ascii 128))
  (target-asset-classes (list 10 (string-ascii 32)))
  (initial-value-locked uint)
)
  (let
    (
      (new-allocation-id (+ (var-get master-allocation-counter) u1))
      (calculated-horizon (+ block-height rebalancing-duration))
    )
    ;; Comprehensive input validation with detailed error feedback mechanisms
    (asserts! (> (len distribution-label) u0) distribution-parameters-invalid)
    (asserts! (< (len distribution-label) u65) distribution-parameters-invalid)
    (asserts! (> allocation-percentage u0) investment-window-closed)
    (asserts! (<= allocation-percentage u10000) investment-window-closed)
    (asserts! (> rebalancing-duration u0) investment-window-closed)
    (asserts! (< rebalancing-duration u2000000) investment-window-closed)
    (asserts! (> (len investment-thesis) u0) distribution-parameters-invalid)
    (asserts! (< (len investment-thesis) u129) distribution-parameters-invalid)
    (asserts! (verify-asset-class-collection target-asset-classes) unsupported-asset-type)
    (asserts! (> initial-value-locked u0) distribution-parameters-invalid)

    ;; Register treasury allocation in central distribution vault
    (map-insert treasury-distribution-vault
      { allocation-id: new-allocation-id }
      {
        distribution-label: distribution-label,
        treasury-manager: tx-sender,
        allocation-percentage: allocation-percentage,
        distribution-genesis: block-height,
        rebalancing-horizon: calculated-horizon,
        investment-thesis: investment-thesis,
        target-asset-classes: target-asset-classes
      }
    )

    ;; Initialize performance metrics tracking for the new allocation
    (map-insert asset-performance-metrics
      { allocation-id: new-allocation-id }
      {
        total-value-locked: initial-value-locked,
        performance-score: u100,
        risk-assessment: u50,
        last-rebalance: block-height
      }
    )

    ;; Establish foundational access permissions for allocation creator
    (map-insert treasury-access-registry
      { allocation-id: new-allocation-id, accessor-principal: tx-sender }
      { permission-level: u100 }
    )

    ;; Increment global counter for subsequent allocation registrations
    (var-set master-allocation-counter new-allocation-id)
    (ok new-allocation-id)
  )
)

;; Comprehensive treasury allocation modification system with extensive validation
(define-public (update-allocation-parameters
  (allocation-id uint)
  (updated-distribution-label (string-ascii 64))
  (updated-allocation-percentage uint)
  (updated-investment-thesis (string-ascii 128))
  (updated-target-assets (list 10 (string-ascii 32)))
)
  (let
    (
      (current-allocation (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
    )
    ;; Authorization verification and comprehensive parameter validation protocols
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! (is-eq (get treasury-manager current-allocation) tx-sender) allocation-authority-failed)
    (asserts! (> (len updated-distribution-label) u0) distribution-parameters-invalid)
    (asserts! (< (len updated-distribution-label) u65) distribution-parameters-invalid)
    (asserts! (> updated-allocation-percentage u0) investment-window-closed)
    (asserts! (<= updated-allocation-percentage u10000) investment-window-closed)
    (asserts! (> (len updated-investment-thesis) u0) distribution-parameters-invalid)
    (asserts! (< (len updated-investment-thesis) u129) distribution-parameters-invalid)
    (asserts! (verify-asset-class-collection updated-target-assets) unsupported-asset-type)

    ;; Execute comprehensive allocation record update with merged configuration
    (map-set treasury-distribution-vault
      { allocation-id: allocation-id }
      (merge current-allocation {
        distribution-label: updated-distribution-label,
        allocation-percentage: updated-allocation-percentage,
        investment-thesis: updated-investment-thesis,
        target-asset-classes: updated-target-assets
      })
    )
    (ok true)
  )
)

;; Treasury rebalancing window extension with horizon recalculation mechanisms
(define-public (extend-rebalancing-window (allocation-id uint) (additional-duration uint))
  (let
    (
      (allocation-record (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
      (new-rebalancing-horizon (+ (get rebalancing-horizon allocation-record) additional-duration))
    )
    ;; Verify management authority and validate extension parameters
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! (is-eq (get treasury-manager allocation-record) tx-sender) allocation-authority-failed)
    (asserts! (> additional-duration u0) investment-window-closed)
    (asserts! (< additional-duration u2000000) investment-window-closed)

    ;; Update rebalancing horizon with extended temporal window
    (map-set treasury-distribution-vault
      { allocation-id: allocation-id }
      (merge allocation-record { rebalancing-horizon: new-rebalancing-horizon })
    )
    (ok true)
  )
)




;; Performance metrics update system for tracking allocation effectiveness
(define-public (update-performance-metrics
  (allocation-id uint)
  (new-total-value uint)
  (performance-score uint)
  (risk-score uint)
)
  (let
    (
      (current-metrics (unwrap! (map-get? asset-performance-metrics { allocation-id: allocation-id })
        asset-allocation-missing))
    )
    ;; Verify authority and validate performance parameters
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! (validate-permission-level allocation-id tx-sender u75) treasury-access-denied)
    (asserts! (> new-total-value u0) distribution-parameters-invalid)
    (asserts! (<= performance-score u1000) distribution-parameters-invalid)
    (asserts! (<= risk-score u100) distribution-parameters-invalid)

    ;; Update comprehensive performance metrics
    (map-set asset-performance-metrics
      { allocation-id: allocation-id }
      (merge current-metrics {
        total-value-locked: new-total-value,
        performance-score: performance-score,
        risk-assessment: risk-score,
        last-rebalance: block-height
      })
    )
    (ok true)
  )
)



;; Permanent treasury allocation removal with comprehensive security protocols
(define-public (dissolve-treasury-allocation (allocation-id uint))
  (let
    (
      (target-allocation (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
    )
    ;; Comprehensive ownership validation before irreversible deletion
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! (is-eq (get treasury-manager target-allocation) tx-sender) allocation-authority-failed)

    ;; Execute permanent allocation removal from treasury distribution vault
    (map-delete treasury-distribution-vault { allocation-id: allocation-id })
    (map-delete asset-performance-metrics { allocation-id: allocation-id })
    (ok true)
  )
)

;; ===== Advanced read-only functions for comprehensive treasury data retrieval =====

;; Comprehensive allocation information retrieval with access control enforcement
(define-read-only (fetch-allocation-details (allocation-id uint))
  (let
    (
      (allocation-data (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
      (access-authorized (validate-permission-level allocation-id tx-sender u25))
      (is-manager (is-eq (get treasury-manager allocation-data) tx-sender))
    )
    ;; Verify access permissions before comprehensive data retrieval
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! (or access-authorized is-manager) restricted-treasury-operation)

    ;; Return comprehensive allocation information with complete metadata
    (ok {
      distribution-label: (get distribution-label allocation-data),
      treasury-manager: (get treasury-manager allocation-data),
      allocation-percentage: (get allocation-percentage allocation-data),
      distribution-genesis: (get distribution-genesis allocation-data),
      rebalancing-horizon: (get rebalancing-horizon allocation-data),
      investment-thesis: (get investment-thesis allocation-data),
      target-asset-classes: (get target-asset-classes allocation-data),
      rebalancing-window-active: (check-rebalancing-window allocation-id)
    })
  )
)

;; Dynamic allocation percentage calculation considering temporal validity
(define-read-only (calculate-effective-allocation (allocation-id uint))
  (let
    (
      (allocation-record (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
    )
    ;; Return allocation percentage only if rebalancing window remains active
    (if (check-rebalancing-window allocation-id)
      (ok (get allocation-percentage allocation-record))
      (ok u0)
    )
  )
)

;; Performance metrics retrieval with comprehensive analytics data
(define-read-only (retrieve-performance-analytics (allocation-id uint))
  (let
    (
      (metrics-data (unwrap! (map-get? asset-performance-metrics { allocation-id: allocation-id })
        asset-allocation-missing))
      (has-access (validate-permission-level allocation-id tx-sender u50))
    )
    ;; Verify sufficient permission level for performance data access
    (asserts! (validate-allocation-exists allocation-id) asset-allocation-missing)
    (asserts! has-access treasury-access-denied)

    ;; Return comprehensive performance metrics with analytics
    (ok {
      total-value-locked: (get total-value-locked metrics-data),
      performance-score: (get performance-score metrics-data),
      risk-assessment: (get risk-assessment metrics-data),
      last-rebalance: (get last-rebalance metrics-data),
      allocation-active: (check-rebalancing-window allocation-id)
    })
  )
)

;; Treasury system overview and administrative statistics
(define-read-only (generate-treasury-overview)
  (ok {
    total-active-allocations: (var-get master-allocation-counter),
    primary-treasury-controller: primary-treasury-controller,
    current-block-height: block-height
  })
)

;; Treasury management authority verification utility
(define-read-only (verify-treasury-manager (allocation-id uint))
  (match (map-get? treasury-distribution-vault { allocation-id: allocation-id })
    allocation-data (ok (get treasury-manager allocation-data))
    asset-allocation-missing
  )
)

;; Comprehensive access permission evaluation and authorization assessment
(define-read-only (evaluate-treasury-permissions (allocation-id uint) (permission-requester principal))
  (let
    (
      (allocation-data (unwrap! (map-get? treasury-distribution-vault { allocation-id: allocation-id })
        asset-allocation-missing))
      (permission-level (default-to u0
        (get permission-level
          (map-get? treasury-access-registry { allocation-id: allocation-id, accessor-principal: permission-requester })
        )
      ))
    )
    ;; Return comprehensive authorization status with detailed permission analysis
    (ok {
      current-permission-level: permission-level,
      is-treasury-manager: (is-eq (get treasury-manager allocation-data) permission-requester),
      can-read-basic-info: (or (>= permission-level u25) (is-eq (get treasury-manager allocation-data) permission-requester)),
      can-read-performance: (or (>= permission-level u50) (is-eq (get treasury-manager allocation-data) permission-requester)),
      can-update-metrics: (or (>= permission-level u75) (is-eq (get treasury-manager allocation-data) permission-requester)),
      rebalancing-window-status: (check-rebalancing-window allocation-id)
    })
  )
)