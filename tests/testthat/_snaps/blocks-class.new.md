# blocks utils

    Code
      print(ab)
    Output
      <blocks[2]>
      
      a
       <dataset_block<data_block<block>>>
       Name: "Dataset"
       No data inputs
       Initial block state:
        $ dataset: chr(0)
        $ package: chr "datasets"
       Constructor: blockr.core::new_dataset_block()
      
      b
       <subset_block<transform_block<block>>>
       Name: "Subset"
       Data inputs: "data"
       Initial block state:
        $ subset: chr ""
        $ select: chr ""
       Constructor: blockr.core::new_subset_block()

