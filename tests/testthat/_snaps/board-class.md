# board constructor

    Code
      print(board)
    Output
      <board>
      ID: "my_board"
      
      Blocks[5]:
      
      d<merge_block<block>>
      Name: "Merge"
      Data inputs: "x" and "y"
      Block state:
       $ all_x: logi FALSE
       $ all_y: logi FALSE
      
      a<dataset_block<block>>
      Name: "Dataset"
      No data inputs
      Block state:
       $ package: chr "datasets"
      
      c<subset_block<block>>
      Name: "Subset"
      Data inputs: "data"
      Block state:
       $ subset: chr ""
       $ select: chr ""
      
      e<subset_block<block>>
      Name: "Subset"
      Data inputs: "data"
      Block state:
       $ subset: chr ""
       $ select: chr ""
      
      b<dataset_block<block>>
      Name: "Dataset"
      No data inputs
      Block state:
       $ package: chr "datasets"
      
      Links[4]:
      
      ad: a -> d (x)
      cd: c -> d (y)
      bc: b -> c (data)
      de: d -> e (data)
      
      Stacks[1]:
      
      bc<stack[2]>
      Name: "Stack"
      Blocks: "b" and "c"
      
      Options:
      <board_options[2]>
        show_conditions: warning, error
        dark_mode: NULL

