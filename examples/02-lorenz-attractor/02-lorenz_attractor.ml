module LorenzAttractor = struct
  (* Constants *)
  let sigma = 10.0
  let rho = 28.0
  let beta = 8.0 /. 3.0

  (* Types *)
  type point = float * float * float

  (* Helper function to calculate the next state *)
  let next_state ((x, y, z) : point) (dt : float) : point =
    let dx = sigma *. (y -. x) in
    let dy = x *. (rho -. z) -. y in
    let dz = x *. y -. beta *. z in
    (x +. dx *. dt, y +. dy *. dt, z +. dz *. dt)

  (* Simulate the Lorenz attractor *)
  let simulate (num_points : int) (dt : float) : point list =
    let initial_state = (1.0, 1.0, 1.0) in
    let rec simulate_helper state n acc =
      if n = 0 then List.rev acc
      else
        let next = next_state state dt in
        simulate_helper next (n - 1) (next :: acc)
    in
    simulate_helper initial_state num_points []

  (* Format points for Gnuplot *)
  let format_points (points : point list) : string =
    points
    |> List.map (fun (x, y, z) -> Printf.sprintf "%f %f %f\n" x y z)
    |> String.concat ""

  (* Plot the attractor using Gnuplot *)
  let plot (points : point list) : unit =
    (* Write data to file *)
    let oc = open_out "lorenz_data.dat" in
    output_string oc (format_points points);
    close_out oc;

    (* Create Gnuplot commands *)
    let gnuplot_commands = "
      set term png
      set output 'lorenz_attractor.png'
      set title 'Lorenz Attractor'
      set xlabel 'X'
      set ylabel 'Y'
      set zlabel 'Z'
      splot 'lorenz_data.dat' with lines notitle
    " in

    (* Write Gnuplot commands to file *)
    let oc = open_out "plot_commands.gp" in
    output_string oc gnuplot_commands;
    close_out oc;

    (* Execute Gnuplot *)
    ignore (Sys.command "gnuplot plot_commands.gp")
end

(* Usage *)
let () =
  let num_points = 10000 in
  let dt = 0.01 in
  let points = LorenzAttractor.simulate num_points dt in
  LorenzAttractor.plot points;
  print_endline "Lorenz attractor plot saved as 'lorenz_attractor.png'"