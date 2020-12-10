let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let preamble_size = 25

let first_invalid (xs: int array): int =
  let exception Result of int in
  let n = Array.length xs in
  try
    for i = preamble_size to n - 1 do
      let x = Array.get xs i in
      let is_valid (): bool =
        try
          for a = i - preamble_size to i - 2 do
            for b = a + 1 to i - 1 do
              let a' = Array.get xs a in
              let b' = Array.get xs b in
              if a' + b' == x then
                raise_notrace Exit
            done
          done;
          false
        with
          Exit -> true
      in
      if not (is_valid ()) then
        raise_notrace (Result x)
    done;
    assert false                (* unreachable *)
  with
    Result x -> x

let part_1 = first_invalid

let part_2 (xs: int array): int =
  let exception Result of int in
  let n = Array.length xs in
  let fi = first_invalid xs in
  let ys = Array.make n 0 in
  Array.set ys 0 (Array.get xs 0);
  for i = 1 to n - 1 do
    Array.set ys i (Array.get ys (i - 1) + Array.get xs i)
  done;
  try
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        let s =
          if i == 0
          then Array.get ys j
          else Array.get ys j - Array.get ys (i - 1)
        in
        if s == fi then
          let mx = ref Int.min_int in
          let mn = ref Int.max_int in
          for k = i to j do
            mx := max (Array.get xs k) !mx;
            mn := min (Array.get xs k) !mn
          done;
          raise_notrace (Result (!mx + !mn))
      done
    done;
    assert false
  with
    Result x -> x

let solve_file (file_path: string) =
  let xs =
    file_path
    |> read_whole_file
    |> String.split_on_char '\n'
    |> List.map (String.trim)
    |> List.filter (fun s -> String.length s > 0)
    |> List.map int_of_string
    |> Array.of_list
  in
  Printf.printf "Input file: %s\n" file_path;
  Printf.printf "Part 1: %d\n" (part_1 xs);
  Printf.printf "Part 2: %d\n" (part_2 xs)

let () =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.iter solve_file
