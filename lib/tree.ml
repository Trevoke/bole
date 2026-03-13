let build ?(target_size = 64) store pairs =
  ignore target_size;
  ignore pairs;
  let empty_leaf = Chunk.encode (Chunk.Leaf []) in
  Store.put store empty_leaf
