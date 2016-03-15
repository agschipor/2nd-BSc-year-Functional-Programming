data MyTree = MyLeaf Double | MyOp Char MyTree MyTree
			
countLeaves (MyLeaf _ ) = 1
countLeaves (MyOp _ l r ) = countLeaves l + countLeaves r

countOp (MyLeaf _ ) = 0
countOp (MyOp _ l r ) = 1 + countOp l + countOp r

--evalTree (MyLeaf x) = x
--evalTree (MyOp '+' l r) = (evalTree l) + (evalTree r)
--evalTree (MyOp '-' l r) = (evalTree l) - (evalTree r)
--evalTree (MyOp '*' l r) = (evalTree l) * (evalTree r)
--evalTree (MyOp '/' l r) = (evalTree l) / (evalTree r)

denotation '+' = (+)
denotation '-' = (-)
denotation '*' = (*)
denotation '/' = (/)

evalTree (MyLeaf x) = x
evalTree (MyOp '*' l r) = if(eval l == 0) then 0 else (eval l) * (eval r)
evalTree (MyOp op l r) = (denotation op) (evalTree l) (evalTree r)

ai x = MyOp '+' (ai(x+1)) (MyLeaf x)