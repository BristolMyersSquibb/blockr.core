# block constructor

    Code
      print(board)
    Output
      <board>
      
      Blocks[5]:
      
      d<merge_block<transform_block<block>>>
      Name: "Merge block"
      Data inputs: "x" and "y"
      Initial block state:
       $ by   : chr(0)
       $ all_x: logi FALSE
       $ all_y: logi FALSE
      Constructor: blockr.core::new_merge_block()
      
      a<dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      c<subset_block<transform_block<block>>>
      Name: "Subset block"
      Data inputs: "data"
      Initial block state:
       $ subset: chr ""
       $ select: chr ""
      Constructor: blockr.core::new_subset_block()
      
      e<subset_block<transform_block<block>>>
      Name: "Subset block"
      Data inputs: "data"
      Initial block state:
       $ subset: chr ""
       $ select: chr ""
      Constructor: blockr.core::new_subset_block()
      
      b<dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      Links[4]:
      
      ad: a -> d (x)
      cd: c -> d (y)
      bc: b -> c (data)
      de: d -> e (data)
      
      Stacks[1]:
      
      bc<stack[2]>
      Name: "Stack 1"
      Blocks: "b" and "c"
      
      Options:
      <board_options[7]>
        board_name: Board
        show_conditions: warning, error
        n_rows: 50
        page_size: 5
        filter_rows: FALSE
        thematic: NULL
        dark_mode: NULL

---

    Code
      print(rm_blocks(board, "c"))
    Output
      <board>
      
      Blocks[4]:
      
      d<merge_block<transform_block<block>>>
      Name: "Merge block"
      Data inputs: "x" and "y"
      Initial block state:
       $ by   : chr(0)
       $ all_x: logi FALSE
       $ all_y: logi FALSE
      Constructor: blockr.core::new_merge_block()
      
      a<dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      e<subset_block<transform_block<block>>>
      Name: "Subset block"
      Data inputs: "data"
      Initial block state:
       $ subset: chr ""
       $ select: chr ""
      Constructor: blockr.core::new_subset_block()
      
      b<dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      Links[2]:
      
      ad: a -> d (x)
      de: d -> e (data)
      
      Stacks[1]:
      
      bc<stack[1]>
      Name: "Stack 1"
      Blocks: "b"
      
      Options:
      <board_options[7]>
        board_name: Board
        show_conditions: warning, error
        n_rows: 50
        page_size: 5
        filter_rows: FALSE
        thematic: NULL
        dark_mode: NULL

