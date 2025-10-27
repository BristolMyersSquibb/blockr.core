# block class

    Code
      print(x)
    Output
      <dataset_block<data_block<block>>>
      Name: "Dataset"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()

# Without package blocks can print

    Code
      blk()
    Output
      <dummy_block<data_block<block>>>
      Name: "Dummy"
      No data inputs
      Initial block state:
       $ text: chr "Hello World"
      Constructor: <local function>

