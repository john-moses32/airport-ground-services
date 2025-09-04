;; Airport Ground Services Smart Contract
;; Manages equipment scheduling, crew coordination, and service quality tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-rating (err u103))
(define-constant err-equipment-busy (err u104))
(define-constant err-crew-unavailable (err u105))
(define-constant err-unauthorized (err u106))

;; Data Variables
(define-data-var next-equipment-id uint u1)
(define-data-var next-crew-id uint u1)
(define-data-var next-service-id uint u1)

;; Equipment Data Structure
(define-map equipment 
  { equipment-id: uint }
  {
    name: (string-ascii 50),
    equipment-type: (string-ascii 30),
    status: (string-ascii 20),
    location: (string-ascii 50),
    last-maintenance: uint,
    assigned-to: (optional uint)
  }
)

;; Crew Data Structure
(define-map crew-members
  { crew-id: uint }
  {
    name: (string-ascii 50),
    role: (string-ascii 30),
    certification-level: uint,
    status: (string-ascii 20),
    current-assignment: (optional uint),
    total-services: uint
  }
)

;; Service Records
(define-map service-records
  { service-id: uint }
  {
    flight-number: (string-ascii 20),
    service-type: (string-ascii 30),
    equipment-id: uint,
    crew-id: uint,
    start-time: uint,
    end-time: (optional uint),
    status: (string-ascii 20),
    quality-rating: (optional uint),
    notes: (string-ascii 200)
  }
)

;; Equipment Schedules
(define-map equipment-schedule
  { equipment-id: uint, date: uint }
  {
    scheduled-services: (list 10 uint),
    maintenance-window: (optional { start: uint, end: uint })
  }
)

;; Quality Metrics
(define-map service-quality
  { crew-id: uint }
  {
    total-ratings: uint,
    rating-sum: uint,
    average-rating: uint,
    services-completed: uint
  }
)

;; Public Functions

;; Register new equipment
(define-public (register-equipment (name (string-ascii 50)) (equipment-type (string-ascii 30)) (location (string-ascii 50)))
  (let ((equipment-id (var-get next-equipment-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set equipment
      { equipment-id: equipment-id }
      {
        name: name,
        equipment-type: equipment-type,
        status: "available",
        location: location,
        last-maintenance: stacks-block-height,
        assigned-to: none
      }
    )
    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

;; Register new crew member
(define-public (register-crew-member (name (string-ascii 50)) (role (string-ascii 30)) (cert-level uint))
  (let ((crew-id (var-get next-crew-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crew-members
      { crew-id: crew-id }
      {
        name: name,
        role: role,
        certification-level: cert-level,
        status: "available",
        current-assignment: none,
        total-services: u0
      }
    )
    (map-set service-quality
      { crew-id: crew-id }
      {
        total-ratings: u0,
        rating-sum: u0,
        average-rating: u0,
        services-completed: u0
      }
    )
    (var-set next-crew-id (+ crew-id u1))
    (ok crew-id)
  )
)

;; Schedule a service
(define-public (schedule-service 
  (flight-number (string-ascii 20))
  (service-type (string-ascii 30))
  (equipment-id uint)
  (crew-id uint)
  (start-time uint)
)
  (let (
    (service-id (var-get next-service-id))
    (equipment-info (unwrap! (map-get? equipment { equipment-id: equipment-id }) err-not-found))
    (crew-info (unwrap! (map-get? crew-members { crew-id: crew-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status equipment-info) "available") err-equipment-busy)
    (asserts! (is-eq (get status crew-info) "available") err-crew-unavailable)
    
    ;; Create service record
    (map-set service-records
      { service-id: service-id }
      {
        flight-number: flight-number,
        service-type: service-type,
        equipment-id: equipment-id,
        crew-id: crew-id,
        start-time: start-time,
        end-time: none,
        status: "scheduled",
        quality-rating: none,
        notes: ""
      }
    )
    
    ;; Update equipment status
    (map-set equipment
      { equipment-id: equipment-id }
      (merge equipment-info { status: "assigned", assigned-to: (some service-id) })
    )
    
    ;; Update crew status
    (map-set crew-members
      { crew-id: crew-id }
      (merge crew-info { status: "assigned", current-assignment: (some service-id) })
    )
    
    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

;; Complete a service
(define-public (complete-service (service-id uint) (end-time uint) (notes (string-ascii 200)))
  (let (
    (service-info (unwrap! (map-get? service-records { service-id: service-id }) err-not-found))
    (equipment-id (get equipment-id service-info))
    (crew-id (get crew-id service-info))
    (equipment-info (unwrap! (map-get? equipment { equipment-id: equipment-id }) err-not-found))
    (crew-info (unwrap! (map-get? crew-members { crew-id: crew-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update service record
    (map-set service-records
      { service-id: service-id }
      (merge service-info { end-time: (some end-time), status: "completed", notes: notes })
    )
    
    ;; Free equipment
    (map-set equipment
      { equipment-id: equipment-id }
      (merge equipment-info { status: "available", assigned-to: none })
    )
    
    ;; Free crew member and update service count
    (map-set crew-members
      { crew-id: crew-id }
      (merge crew-info { 
        status: "available", 
        current-assignment: none,
        total-services: (+ (get total-services crew-info) u1)
      })
    )
    
    (ok true)
  )
)

;; Rate service quality
(define-public (rate-service (service-id uint) (rating uint))
  (let (
    (service-info (unwrap! (map-get? service-records { service-id: service-id }) err-not-found))
    (crew-id (get crew-id service-info))
    (quality-info (unwrap! (map-get? service-quality { crew-id: crew-id }) err-not-found))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (is-eq (get status service-info) "completed") err-unauthorized)
    (asserts! (is-none (get quality-rating service-info)) err-already-exists)
    
    ;; Update service record with rating
    (map-set service-records
      { service-id: service-id }
      (merge service-info { quality-rating: (some rating) })
    )
    
    ;; Update quality metrics
    (let (
      (new-total-ratings (+ (get total-ratings quality-info) u1))
      (new-rating-sum (+ (get rating-sum quality-info) rating))
      (new-average (/ new-rating-sum new-total-ratings))
      (new-completed (+ (get services-completed quality-info) u1))
    )
      (map-set service-quality
        { crew-id: crew-id }
        {
          total-ratings: new-total-ratings,
          rating-sum: new-rating-sum,
          average-rating: new-average,
          services-completed: new-completed
        }
      )
    )
    
    (ok true)
  )
)

;; Update equipment maintenance
(define-public (update-equipment-maintenance (equipment-id uint))
  (let ((equipment-info (unwrap! (map-get? equipment { equipment-id: equipment-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set equipment
      { equipment-id: equipment-id }
      (merge equipment-info { last-maintenance: stacks-block-height })
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get equipment details
(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment { equipment-id: equipment-id })
)

;; Get crew member details
(define-read-only (get-crew-member (crew-id uint))
  (map-get? crew-members { crew-id: crew-id })
)

;; Get service details
(define-read-only (get-service (service-id uint))
  (map-get? service-records { service-id: service-id })
)

;; Get crew quality metrics
(define-read-only (get-crew-quality (crew-id uint))
  (map-get? service-quality { crew-id: crew-id })
)

;; Get current counters
(define-read-only (get-counters)
  {
    next-equipment-id: (var-get next-equipment-id),
    next-crew-id: (var-get next-crew-id),
    next-service-id: (var-get next-service-id)
  }
)


;; title: airport-services
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

