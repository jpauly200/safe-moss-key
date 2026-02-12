;; SafeMossKey - Zero-Knowledge Skill Verification Platform
;; A decentralized identity and skill verification system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-skill (err u105))
(define-constant err-invalid-proof (err u106))

;; Token configuration
(define-fungible-token moss-token)
(define-constant token-decimals u6)
(define-constant initial-supply u1000000000000) ;; 1M tokens with 6 decimals

;; Data Variables
(define-data-var verification-fee uint u1000) ;; Fee in MOSS tokens
(define-data-var validator-reward uint u100) ;; Reward for validators
(define-data-var total-verifications uint u0)

;; Data Maps
;; Skill Registry: skill-id -> skill metadata
(define-map skill-registry
  { skill-id: uint }
  {
    name: (string-ascii 64),
    category: (string-ascii 32),
    difficulty-level: uint,
    active: bool,
    created-at: uint
  }
)

;; User Skills: (user, skill-id) -> verification data
(define-map user-skills
  { user: principal, skill-id: uint }
  {
    verified: bool,
    proof-hash: (buff 32),
    verification-count: uint,
    score: uint,
    last-verified: uint
  }
)

;; Reputation Scores: user -> reputation metadata
(define-map reputation-scores
  { user: principal }
  {
    total-score: uint,
    verification-count: uint,
    skill-count: uint,
    joined-at: uint,
    last-updated: uint
  }
)

;; Validators: user -> validator status
(define-map validators
  { validator: principal }
  {
    active: bool,
    total-validations: uint,
    reputation: uint,
    staked-amount: uint
  }
)

;; Pending Verifications: verification-id -> verification request
(define-map pending-verifications
  { verification-id: uint }
  {
    user: principal,
    skill-id: uint,
    proof-hash: (buff 32),
    validator: (optional principal),
    status: (string-ascii 16),
    created-at: uint
  }
)

;; Counters
(define-data-var next-skill-id uint u1)
(define-data-var next-verification-id uint u1)

;; Initialize contract with token supply
(begin
  (ft-mint? moss-token initial-supply contract-owner)
)

;; Read-only functions

(define-read-only (get-skill (skill-id uint))
  (map-get? skill-registry { skill-id: skill-id })
)

(define-read-only (get-user-skill (user principal) (skill-id uint))
  (map-get? user-skills { user: user, skill-id: skill-id })
)

(define-read-only (get-reputation (user principal))
  (default-to
    { total-score: u0, verification-count: u0, skill-count: u0, joined-at: u0, last-updated: u0 }
    (map-get? reputation-scores { user: user })
  )
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validators { validator: validator })
)

(define-read-only (get-token-balance (account principal))
  (ok (ft-get-balance moss-token account))
)

(define-read-only (get-verification-fee)
  (ok (var-get verification-fee))
)

(define-read-only (get-total-verifications)
  (ok (var-get total-verifications))
)

;; Public functions - Skill Management

(define-public (register-skill (name (string-ascii 64)) (category (string-ascii 32)) (difficulty uint))
  (let
    (
      (skill-id (var-get next-skill-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set skill-registry
      { skill-id: skill-id }
      {
        name: name,
        category: category,
        difficulty-level: difficulty,
        active: true,
        created-at: block-height
      }
    )
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)
  )
)

(define-public (toggle-skill-status (skill-id uint))
  (let
    (
      (skill (unwrap! (map-get? skill-registry { skill-id: skill-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set skill-registry
      { skill-id: skill-id }
      (merge skill { active: (not (get active skill)) })
    )
    (ok true)
  )
)

;; Validator Management

(define-public (register-validator (stake-amount uint))
  (let
    (
      (current-balance (ft-get-balance moss-token tx-sender))
    )
    (asserts! (>= current-balance stake-amount) err-insufficient-balance)
    (try! (ft-transfer? moss-token stake-amount tx-sender (as-contract tx-sender)))
    (map-set validators
      { validator: tx-sender }
      {
        active: true,
        total-validations: u0,
        reputation: u100,
        staked-amount: stake-amount
      }
    )
    (ok true)
  )
)

(define-public (unregister-validator)
  (let
    (
      (validator-info (unwrap! (map-get? validators { validator: tx-sender }) err-not-found))
      (stake (get staked-amount validator-info))
    )
    (try! (as-contract (ft-transfer? moss-token stake tx-sender tx-sender)))
    (map-delete validators { validator: tx-sender })
    (ok true)
  )
)

;; Verification System

(define-public (submit-verification (skill-id uint) (proof-hash (buff 32)))
  (let
    (
      (verification-id (var-get next-verification-id))
      (skill (unwrap! (map-get? skill-registry { skill-id: skill-id }) err-invalid-skill))
      (fee (var-get verification-fee))
    )
    (asserts! (get active skill) err-invalid-skill)
    (try! (ft-transfer? moss-token fee tx-sender (as-contract tx-sender)))
    
    (map-set pending-verifications
      { verification-id: verification-id }
      {
        user: tx-sender,
        skill-id: skill-id,
        proof-hash: proof-hash,
        validator: none,
        status: "pending",
        created-at: block-height
      }
    )
    (var-set next-verification-id (+ verification-id u1))
    (ok verification-id)
  )
)

(define-public (validate-verification (verification-id uint) (approved bool))
  (let
    (
      (verification (unwrap! (map-get? pending-verifications { verification-id: verification-id }) err-not-found))
      (validator-info (unwrap! (map-get? validators { validator: tx-sender }) err-unauthorized))
      (user (get user verification))
      (skill-id (get skill-id verification))
      (current-skill (default-to
        { verified: false, proof-hash: 0x00, verification-count: u0, score: u0, last-verified: u0 }
        (map-get? user-skills { user: user, skill-id: skill-id })
      ))
      (current-rep (get-reputation user))
    )
    (asserts! (get active validator-info) err-unauthorized)
    
    (if approved
      (begin
        ;; Update user skill
        (map-set user-skills
          { user: user, skill-id: skill-id }
          {
            verified: true,
            proof-hash: (get proof-hash verification),
            verification-count: (+ (get verification-count current-skill) u1),
            score: (+ (get score current-skill) u10),
            last-verified: block-height
          }
        )
        
        ;; Update reputation
        (map-set reputation-scores
          { user: user }
          {
            total-score: (+ (get total-score current-rep) u10),
            verification-count: (+ (get verification-count current-rep) u1),
            skill-count: (if (get verified current-skill) 
                           (get skill-count current-rep)
                           (+ (get skill-count current-rep) u1)),
            joined-at: (if (is-eq (get joined-at current-rep) u0) block-height (get joined-at current-rep)),
            last-updated: block-height
          }
        )
        
        ;; Reward validator
        (try! (as-contract (ft-transfer? moss-token (var-get validator-reward) tx-sender tx-sender)))
        
        ;; Update validator stats
        (map-set validators
          { validator: tx-sender }
          (merge validator-info { 
            total-validations: (+ (get total-validations validator-info) u1),
            reputation: (+ (get reputation validator-info) u1)
          })
        )
        
        (var-set total-verifications (+ (var-get total-verifications) u1))
      )
      true
    )
    
    ;; Update verification status
    (map-set pending-verifications
      { verification-id: verification-id }
      (merge verification { 
        validator: (some tx-sender),
        status: (if approved "approved" "rejected")
      })
    )
    
    (ok approved)
  )
)

;; Token Transfer

(define-public (transfer-tokens (amount uint) (recipient principal))
  (ft-transfer? moss-token amount tx-sender recipient)
)

;; Admin functions

(define-public (set-verification-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set verification-fee new-fee)
    (ok true)
  )
)

(define-public (set-validator-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set validator-reward new-reward)
    (ok true)
  )
)

(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-mint? moss-token amount recipient)
  )
)
