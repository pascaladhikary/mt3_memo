open Common
open Plsolution
(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)

let rec unify constraints =
  match constraints with
    [] -> Some([])
  | (s,t)::rem_constraints ->
    (* Delete *)
    if s = t then unify rem_constraints
    else
      (match s with 
      
      | TyVar n -> (* Eliminate *)
      	if occurs n t then None else
	      let r = List.map(fun(t1, t2)->(monoTy_lift_subst [n,t] t1, monoTy_lift_subst [n,t] t2)) rem_constraints
	      in
	      (match unify r 
			with None -> None
			| Some phi ->Some((n, monoTy_lift_subst phi t)::phi))

      | TyConst(c, args) ->  (* Orient and Decompose *)
        (match t with 
        | TyVar ty -> unify((t, s)::rem_constraints)
        | TyConst (tc, targs) -> 
          if tc = c then let rec helper l = match l with 
          | ([], []) -> []
          | ((sh::st), (th::tt)) -> (sh,th)::(helper (st, tt))
          | _ -> []
          in unify ((helper(args, targs)) @ rem_constraints)
          else None 
      )
  )
