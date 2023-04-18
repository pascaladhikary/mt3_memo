open Common

let rec occurs v ty = 
  match ty with TyVar x -> x = v | TyConst (y, z) -> List.exists (occurs v) z;;

let rec subst_fun subst tyv =
  match sigma with [] -> TyVar ty | (x, y)::z -> if x = ty then y else subst_fun z ty;;

let rec monoTy_lift_subst sigma ty = 
  match ty with TyVar x -> subst_fun sigma x | TyConst (y, z) -> TyConst (y, List.map (monoTy_lift_subst sigma) z);;
  
