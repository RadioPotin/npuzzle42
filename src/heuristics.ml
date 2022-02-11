open Types

(** [informed_search goal state] compares the current state to the goal one and returns an int corresponding to the number of tiles in [state] that would differ from their expected position in [goal] *)
let informed_search (goal:tile list) (state:tile list) : int =
  let goal =
    Utils.remove_blank goal
  in
  let state =
    Utils.remove_blank state
  in
  List.fold_left2
    (fun acc goal_tile state_tile ->
        if goal_tile = state_tile then acc else acc + 1
    ) 0 goal state
