module SolverImpl where

import Absyn




-- assume the input RProf is well formed, that is , it is sorted by resource name.
combine :: RProf -> RProf -> RProf
combine rp1 rp2 = combineHelper rp1 rp2 []



combineHelper :: RProf -> RProf -> RProf -> RProf
combineHelper [] rp2 container = (reverse container) ++ rp2
combineHelper rp1 [] container = (reverse container) ++ rp1
combineHelper rp1@(pf1@(R r1, (nc1, req1)):rest1) rp2@(pf2@(R r2, (nc2, req2)):rest2) container =
                  case r1 == r2 of
                      True -> case (nc1+nc2, maximum [req1, req2]) of
                                 (0, 0)    -> combineHelper rest1 rest2 container
                                 (nc, req) -> combineHelper rest1 rest2 ((R r1, (nc, req)):container)
                      False -> if r1 < r2
                               then combineHelper rest1 rp2 (pf1:container)
                               else combineHelper rp1 rest2 (pf2:container)



-- 1.check if database is empty
-- 2.check if resource in goal is decalred in database
-- 3.accordingto the solution, select resources from database
-- 4.combine selected resources with goal
-- 5.check result if available >= needed
verify :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify ([], []) g sol =  Left "DB is empty"
verify db g sol = case (checkgoal db g True) of
                      False -> Left "goal contains database undeclased resource"
                      True  -> let selected_prof = select_prof db sol []
                                   result = combineSlectwithGoal g selected_prof
                               in  case checkSolved result True of
                                       False -> Left "solution cannot meet goal"
                                    -- True  -> Right (calcSysRPf selected_prof)
                                    -- return result : because exam question required return the rpf of complete system.
                                       True -> Right result




-- when solution is checked to be valid for the goal,
-- merge the selected_prof and return the final profile
-- calcSysRPf :: RProf -> RProf
-- calcSysRPf (rpf:rest) = combineSlectwithGoal [rpf] rest




-- check if the combined profile is valid
checkSolved :: RProf -> Bool -> Bool
checkSolved [] flag = flag
checkSolved ((res, (available, needed)) : rest) flag =
      case available >= needed of
          True  -> checkSolved rest flag
          False -> False


-- combine all the selected profile with goal and return a final profile
combineSlectwithGoal :: Goal -> RProf -> RProf
combineSlectwithGoal g [] = g
combineSlectwithGoal g (rpf:rest) = let new_g = combine g [rpf] in
                                        combineSlectwithGoal new_g rest


-- according to the list of solution, select profile from database
select_prof :: DB -> Solution -> RProf  -> RProf
select_prof db [] container = container
select_prof (res, (db_cn, rpf):db_rest) sol@((cn, i):rest) container =
       case db_cn == cn of
           True  -> let newcontainer = (concatMap (replicate i) rpf) ++ container in
                       select_prof (res, db_rest) rest newcontainer
           False -> select_prof (res, db_rest) sol container


-- check if there is undeclared resource in goal
checkgoal :: DB -> Goal -> Bool -> Bool
checkgoal db [] flag = flag
checkgoal db@(res, db_rpf) ((g_res, (avai, need)) : rest) flag =
                case (g_res `elem` res) of
                   True  -> checkgoal db rest flag
                   False -> False





-- DB Goal is well formed
-- DB = ([Resource, Resource, ....], [(CName, RProf), (CName, RProf)......])
-- Goal = RProf = [(Resource, (Int, Int)), (Resource, (Int, Int)).....] -- (net contribution, requirement)


--1: check resource in goal is declared in database
--2:

solve :: DB -> Goal -> Int -> Either ErrMsg Solution
solve = undefined
-- solve db@(res, db_rpf) g n =
--       case checkgoal db g True of
--          False -> Left "goal contains undeclared resource"
--          True  -> case (any_solution db_rpf goal (get_partition n res) []) of
--               [] -> Left "goal exhausted"
--               solution -> Right solution


-- -- any_solution :: [(CName, RProf)] -> Goal -> [[Resource]] -> [[Resource]] -> Either ErrMsg Solution
-- -- any_solution cres goal nested_res container =
-- --     case verify

-- -- partitions function is from https://stackoverflow.com/questions/13443237/all-possible-sublists-of-a-list
-- -- used to get all the possible sublist from a list
-- partitions :: [a] -> [[[a]]]
-- partitions [] = [[]]    -- only one partition of an empty list, an empty partition
-- partitions (x:xs) = [[x]:part | part <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]

-- get_partition :: Int -> [a] -> [[a]]
-- get_partition goal source_list =
--     let all_partition = partitions source_list
--     in find_partition goal all_partition []

-- get_length :: (Num b) => [a] -> b
-- get_length [] = 0
-- get_length xs = sum [1 | _ <- xs]

-- find_partition :: Int -> [[a]] -> [[a]] -> [[a]]
-- find_partition goal [] container = container
-- find_partition goal (fst_list : rest) container =
--     case get_length(fst_list) <= goal of
--         True -> find_partition goal rest (fst_list : container)
--         False -> find_partition goal rest container




-- -- check if the combined profile is valid
-- checkSingleSolved :: (Resource, (Int, Int)) -> Bool
-- checkSingleSolved (res, (available, needed)) = available >= needed
