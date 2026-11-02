# a block class holds its fields

    Code
      print(head_class)
    Output
      <block_class> head_block
      Data inputs: "data"
      State args: "n" and "direction"
      Requests: "n_max"
      UI: head-block.js (blockr.core)

# blocks of a class print, combine and report their state args

    Code
      print(blk)
    Output
      <head_block<block>>
      Name: "Head"
      Data inputs: "data"
      Block state:
       $ n        : int 6
       $ direction: chr "head"

---

    Code
      print(new_class_rbind_block())
    Output
      <rbind_block<block>>
      Name: "Rbind"
      Indefinite arity
      Stateless block

---

    Code
      print(new_class_head_block)
    Output
      <block_constructor> head_block
      function (n = 6L, direction = c("head", "tail"), block_name = NULL, 
          visible = NULL) 

