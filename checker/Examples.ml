
let unbound_variable var = Some (Expr.UnboundVariable var)
let permission_denied op = Some (Expr.PermissionDenied op)
let access_dead_path bad reason = Some(Expr.AccessDeadPath(bad, reason))

let examples = Access.[|
    ( "permission/owner-move"
    , [ "let x = 1;"
      ; "let _ = x;"
      ; "0" ]
    , None );
    ( "permission/owner-mut"
    , [ "let x = 1;"
      ; "let _ = &mut x;"
      ; "0" ]
    , None );
    ( "permission/owner-imm"
    , [ "let x = 1;"
      ; "let _ = &x;"
      ; "0" ]
    , None );
    ( "permission/owner-assign"
    , [ "let x = 1;"
      ; "x := 2;"
      ; "0" ]
    , None );

    ( "permission/mut-deref-move"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let _ = *m;"
      ; "0" ]
    , permission_denied (OpMove("m", [Deref])) );
    ( "permission/mut-deref-mut"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let _ = &mut *m;"
      ; "0" ]
    , None );
    ( "permission/mut-deref-imm"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let _ = & *m;"
      ; "0" ]
    , None );
    ( "permission/mut-deref-assign"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "*m := 2;"
      ; "0" ]
    , None );

    ( "permission/imm-deref-move"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "let _ = *i;"
      ; "0" ]
    , permission_denied (OpMove("i", [Deref])) );
    ( "permission/imm-deref-mut"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "let _ = &mut *i;"
      ; "0" ]
    , permission_denied (OpBorrow(Mut, ("i", [Deref]))) );
    ( "permission/imm-deref-imm"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "let _ = & *i;"
      ; "0" ]
    , None );
    ( "permission/imm-deref-assign"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "*i := 2;"
      ; "0" ]
    , permission_denied (OpAssign("i", [Deref])) );

    ( "basic/move-move"
    , [ "let x = 1;"
      ; "let _ = x;"
      ; "let _ = x;"
      ; "0" ]
    , access_dead_path (OpMove("x", [])) (OpMove("x", [])) );
    ( "basic/imm-move"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "let _ = x;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("x", [])));
    ( "basic/mut-move"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let _ = x;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpMove("x", [])) );
    ( "basic/imm-mut"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "let _ = &mut x;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", []))
            (OpBorrow(Mut, ("x", []))) );
    ( "basic/imm-imm"
    , [ "let x = 1;"
      ; "let i1 = &x;"
      ; "let i2 = &x;"
      ; "let ii = i1;"
      ; "0" ]
    , None );
    ( "basic/mut-imm"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let _ = &x;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", []))
            (OpBorrow(Imm, ("x", []))) );
    ( "basic/mut-mut"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let _ = &mut x;"
      ; "let _ = m1;"
      ; "0" ]
    , access_dead_path (OpMove("m1", []))
            (OpBorrow(Mut, ("x", []))) );
    ( "basic/imm-assign"
    , [ "let x = 1;"
      ; "let i = &x;"
      ; "x := 2;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpAssign("x", [])) );
    ( "basic/mut-assign"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "x := 2;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpAssign("x", [])) );
    ( "basic/move-assign"
    , [ "let x = 1;"
      ; "let _ = x;"
      ; "x := 2;"
      ; "let _ = x;"
      ; "0" ]
    , None );

    ( "nested/mutmut-move"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut m1;"
      ; "let y = x;"
      ; "let mm = m2;"
      ; "0" ]
    , access_dead_path (OpMove("m2", [])) (OpMove("x", [])) );
    ( "nested/immmut-imm"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let im = &m;"
      ; "let _ = &x;"
      ; "let _ = im;"
      ; "0" ]
    , access_dead_path (OpMove("im", []))
            (OpBorrow(Imm, ("x", []))) );

    ( "reborrow/reimm-owner"
    , [ "let x = 1;"
      ; "let i1 = &x;"
      ; "let i2 = & *i1;"
      ; "let _ = x;"
      ; "let _ = i2;"
      ; "0" ]
    , access_dead_path (OpMove("i2", [])) (OpMove("x", [])) );
    ( "rebororw/remut-owner"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let _ = x;"
      ; "let _ = m2;"
      ; "0" ]
    , access_dead_path (OpMove("m2", [])) (OpMove("x", [])) );
    ( "reborrow/remut-move"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let m3 = m1;"
      ; "let mm = m2;"
      ; "0" ]
    , access_dead_path (OpMove("m2", [])) (OpMove("m1", [])) );
    ( "reborrow/remut-remut"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let _ = &mut *m1;"
      ; "let _ = m2;"
      ; "0" ]
    , access_dead_path (OpMove("m2", []))
            (OpBorrow(Mut, ("m1", [Deref]))) );
    ( "reborrow/remut-mut"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let _ = &mut m1;"
      ; "let _ = m2;"
      ; "0" ]
    , access_dead_path (OpMove("m2", []))
            (OpBorrow(Mut, ("m1", []))) );
    ( "reborrow/reremut-owner"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let m3 = &mut *m2;"
      ; "let _ = x;"
      ; "let _ = m3;"
      ; "0" ]
    , access_dead_path (OpMove("m3", [])) (OpMove("x", [])) );
    ( "reborrow/reremut-remut"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let m2 = &mut *m1;"
      ; "let m3 = &mut *m2;"
      ; "let m4 = &mut *m1;"
      ; "let mm = m3;"
      ; "0" ]
    , access_dead_path (OpMove("m3", []))
            (OpBorrow(Mut, ("m1", [Deref]))) );
    ( "reborrow/reimm-remut-imm"
    , [ "let x = 1;"
      ; "let m = &mut x;"
      ; "let i1 = & *m;"
      ; "let _ = &x;"
      ; "let _ = i1;"
      ; "0" ]
    , access_dead_path (OpMove("i1", []))
            (OpBorrow(Imm, ("x", []))) );
    ( "reborrow/mutmut-remut"
    , [ "let x = 1;"
      ; "let m1 = &mut x;"
      ; "let mm = &mut m1;"
      ; "let _ = &mut *m1;"
      ; "let _ = mm;"
      ; "0" ]
    , access_dead_path (OpMove("mm", []))
            (OpBorrow(Mut, ("m1", [Deref]))) );
    ( "reborrow/two-deref-owner"
    , [ "let x = 1;"
      ; "let m0 = &mut x;"
      ; "let mm = &mut m0;"
      ; "let m1 = &mut **mm;"
      ; "let _ = x;"
      ; "let _ = m1;"
      ; "0" ]
    , access_dead_path (OpMove("m1", [])) (OpMove("x", [])) );
    ( "reborrow/two-deref-remut"
    , [ "let x = 1;"
      ; "let m0 = &mut x;"
      ; "let mm = &mut m0;"
      ; "let m1 = &mut **mm;"
      ; "let _ = &mut *m0;"
      ; "let _ = m1;"
      ; "0" ]
    , access_dead_path (OpMove("m1", []))
            (OpBorrow(Mut, ("m0", [Deref]))) );
    ( "reborrow/two-deref-mutmutmut"
    , [ "let x = 1;"
      ; "let m0 = &mut x;"
      ; "let mm = &mut m0;"
      ; "let m1 = &mut **mm;"
      ; "let _ = &mut mm;"
      ; "let _ = m1;"
      ; "0" ]
    , access_dead_path (OpMove("m1", []))
            (OpBorrow(Mut, ("mm", []))) );

    ( "assign/owner-oldval"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "i := &y;"
      ; "let _ = x;"
      ; "let _ = i;"
      ; "0" ]
    , None );
    ( "assign/owner-newval"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "i := &y;"
      ; "let _ = y;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("y", [])) );
    ( "assign/mut-oldval"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "let m = &mut i;"
      ; "*m := &y;"
      ; "let _ = x;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("x", [])) );
    ( "assign/mut-newval"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "let m = &mut i;"
      ; "*m := &y;"
      ; "let _ = y;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("y", [])) );

    ( "tuple/distinct-move"
    , [ "let t = (1, 2);"
      ; "let x = t.1;"
      ; "let y = t.2;"
      ; "let _ = x;"
      ; "0" ]
    , None );
    ( "tuple/distinct-mut"
    , [ "let t = (1, 2);"
      ; "let m0 = &mut t.1;"
      ; "let m1 = &mut t.2;"
      ; "let _ = m0;"
      ; "0" ]
    , None );
    ( "tuple/mut-field-move-whole"
    , [ "let x = 1;"
      ; "let t = (1, &mut x);"
      ; "let _ = x;"
      ; "let _ = t;"
      ; "0" ]
    , access_dead_path (OpMove("t", [])) (OpMove("x", [])) );
    ( "tuple/mut-field-move-distinct"
    , [ "let x = 1;"
      ; "let t = (1, &mut x);"
      ; "let _ = x;"
      ; "let _ = t.1;"
      ; "0" ]
    , None );
    ( "tuple/mut-whole-remut-field"
    , [ "let x = 1;"
      ; "let t = (1, &mut x);"
      ; "let m = &mut t;"
      ; "let _ = &mut *t.2;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", []))
            (OpBorrow(Mut, ("t", [Field 1; Deref]))) );
    ( "tuple/remut-distinct"
    , [ "let t = (1, 2);"
      ; "let m = &mut t;"
      ; "let m0 = &mut (*m).1;"
      ; "let m1 = &mut (*m).2;"
      ; "let _ = m0;"
      ; "0" ]
    , None );
    ( "scope/let-rhs"
    , [ "let x = (let y = 1; 0);"
      ; "y" ]
    , unbound_variable "y" );
    ( "scope/let-ret-borrow"
    , [ "let x = 1;"
      ; "&mut x" ]
    , access_dead_path OpResultLet (OpScope "x") );
    ( "scope/let-ret-reborrow"
    , [ "let x = 1;"
      ; "let _ = (let m = &mut x; &mut *m);"
      ; "0" ]
    , access_dead_path OpResultLet (OpScope "m") );
    ( "scope/let-ret-move"
    , [ "let x = 1;"
      ; "x" ]
    , None );
    ( "scope/let-ret-move-borrow"
    , [ "let x = 1;"
      ; "let _ = (let m = &mut x; m);"
      ; "0" ]
    , None );
    ( "scope/assign-borrow-of-local-owner"
    , [ "let x = 1;"
      ; "let m = &x;"
      ; "let _ = (let y = 2; m := &y);"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpScope "y") );
    ( "scope/assign-borrow-of-local-mut"
    , [ "let x = 1;"
      ; "let m = &x;"
      ; "let mm = &mut m;"
      ; "let _ = (let y = 2; *mm := &y);"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpScope "y") );

    ( "branch/basic"
    , [ "if 0 then 1 else 2" ]
    , None );
    ( "branch/merge-dead-conseq"
    , [ "let x = 1;"
      ; "let _ = (if 0 then x else 0);"
      ; "let _ = x;"
      ; "0" ]
    , access_dead_path (OpMove("x", [])) (OpMove("x", [])) );
    ( "branch/merge-dead-alter"
    , [ "let x = 1;"
      ; "let _ = (if 0 then 0 else x);"
      ; "let _ = x;"
      ; "0" ]
    , access_dead_path (OpMove("x", [])) (OpMove("x", [])) );
    ( "branch/merge-dead-common"
    , [ "let x = 0;"
      ; "let _ = x;"
      ; "let _ = (if 0 then x := 1 else x := 2);"
      ; "let _ = x;"
      ; "0" ]
    , None );
    ( "branch/merge-ret-conseq"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = (if 0 then &x else &y);"
      ; "let _ = x;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("x", [])) );
    ( "branch/merge-ret-alter"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = (if 0 then &x else &y);"
      ; "let _ = y;"
      ; "let _ = i;"
      ; "0" ]
    , access_dead_path (OpMove("i", [])) (OpMove("y", [])) );
    ( "branch/merge-env-conseq"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let z = 3;"
      ; "let m = &mut x;"
      ; "let _ = (if 0 then m := &mut y else m := &mut z);"
      ; "let _ = y;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpMove("y", [])) );
    ( "branch/merge-env-alter"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let z = 3;"
      ; "let m = &mut x;"
      ; "let _ = (if 0 then m := &mut y else m := &mut z);"
      ; "let _ = z;"
      ; "let _ = m;"
      ; "0" ]
    , access_dead_path (OpMove("m", [])) (OpMove("z", [])) );
    ( "branch/merge-env-common"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let z = 3;"
      ; "let m = &mut x;"
      ; "let _ = (if 0 then m := &mut y else m := &mut z);"
      ; "let _ = x;"
      ; "let _ = m;"
      ; "0" ]
    , None );
    ( "branch/mut-invariant-imprecise"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let ix = &x;"
      ; "let iy = &y;"
      ; "let m = (if 0 then &mut ix else &mut iy);"
      ; "let _ = x;"
      ; "let _ = iy;"
      ; "0" ]
    (* This one is now precise! *)
    (* , access_dead_path (OpMove("iy", [])) (OpMove("x", [])) ); *)
    , None );
    ( "branch/imm-covariant-precise"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let ix = &x;"
      ; "let iy = &y;"
      ; "let i = (if 0 then &ix else &iy);"
      ; "let _ = x;"
      ; "let _ = iy;"
      ; "0" ]
    , None );
    ( "branch/dead-ret-independence"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "let i2 ="
      ; "    if 0"
      ; "    then (let _ = x; &y)"
      ; "    else & *i;"
      ; "let _ = i2;"
      ; "0" ]
    , None );
    ( "branch/assign-reborrow-imprecise"
    , [ "let x = 1;"
      ; "let y = 2;"
      ; "let i = &x;"
      ; "let i2 ="
      ; "    if 0"
      ; "    then (i := &y; &x)"
      ; "    else & *i;"
      ; "let _ = y;"
      ; "let _ = i2;"
      ; "0" ]
    , access_dead_path (OpMove("i2", [])) (OpMove("y", [])) );
|] |> Array.map (fun (name, lines, expected) ->
                (name, String.concat "\n" lines, expected))
