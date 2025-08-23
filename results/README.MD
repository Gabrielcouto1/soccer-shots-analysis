### Model Variables

This table outlines the variables used as predictors for each of the five models. The target variable for all models is `is_goal`.

| Variable                    | Model 1 | Model 2 | Model 3 | Model 4 | Model 5 |
| :-------------------------- | :-----: | :-----: | :-----: | :-----: | :-----: |
| `dist_to_goal`              |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `shot_angle`                |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `is_penalty`                |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `is_open_goal`              |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `is_first_time`             |    ✔️    |    ✔️    |    ✔️    |    ✔️    |         |
| `opponents_in_frame`        |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `opponents_in_shot_path`    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `statsbomb_xg`              |    ✔️    |    ✔️    |    ✔️    |    ✔️    |    ✔️    |
| `opponents_in_penalty_area` |         |    ✔️    |         |    ✔️    |         |
| `opponents_in_goal_area`    |         |    ✔️    |         |    ✔️    |         |
| `is_ca`                     |         |         |    ✔️    |         |         |
| `is_from_cross`             |         |         |    ✔️    |         |         |
| `is_under_pressure`         |         |         |    ✔️    |         |         |
| `is_one_on_one`             |         |         |    ✔️    |         |         |
| `goalkeeper_dist`           |         |         |         |    ✔️    |         |
| `closest_opponent_dist`     |         |         |         |         |    ✔️    |