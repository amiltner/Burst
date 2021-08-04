open CoreAndMore

module type Symbol =
sig
  include Data
  val arity : t -> int
end

module type State =
sig
  include Data
  val product : t -> t -> t option
end

module type Reqs =
sig
  include Data

  val partial_compare : t -> t -> partial_order_comparison
end

module type S =
sig
  module Symbol : Symbol
  module State : State
  module Reqs : Reqs

  module Term : sig
    include Data with type t = Symbol.t TimbukSimple.Term.t
  end

  module TermState :
  sig
    include Data with type t = (Symbol.t,State.t) TimbukSimple.TermState.t
  end

  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val equal : t equality_check

  val empty : unit -> t
  val intersect : Symbol.t list -> t -> t -> t
  val copy : t -> t
  val add_transition : t -> Symbol.t -> State.t list -> State.t -> unit
  val remove_transition : t -> Symbol.t -> State.t list -> State.t -> unit
  val states : t -> State.t list
  val final_states : t -> State.t list
  val is_final_state : t -> State.t -> bool
  val add_final_state : t -> State.t -> unit
  val remove_final_state : t -> State.t -> unit
  val has_state : t -> State.t -> bool
  (*val is_empty : t -> bool*)
  (*val accepts_term : t -> Term.t -> bool*)
  val accepting_term_state : t -> Term.t -> TermState.t option
  val transitions_from
    : t
    -> State.t
    -> (Symbol.t * (State.t list) * State.t) list
  val transitions_to : t -> State.t -> (Symbol.t * (State.t list)) list
  val transitions : t -> (Symbol.t * (State.t list) * State.t) list
  val minimize : t -> t
  val size : t -> int
  val min_term_state :
    f:(TermState.t -> bool) ->
    cost:(TermState.t -> Float.t) ->
    reqs:(TermState.t -> Reqs.t) ->
    t ->
    TermState.t option
end

module type Builder =
  functor (Symbol : Symbol) ->
  functor (State : State) ->
  functor (Reqs : Reqs) ->
    (S with module Symbol := Symbol
       and module State := State
       and module Reqs := Reqs)
