
-record(ptr, {type = void, value}).

-import(eqc_c,
        [ free/1
        , create_array/2
        , read_array/2
        , write_array/2
        , read_string/1
        , create_string/1
        , array_index/2, array_index/3
        , deref/1
        , alloc/2, alloc/1
        , store/2
        , cast_ptr/2
        , add_to_ptr/2
        , sizeof/1
        , expand_type/1, type_info/1
        , type_of/1, type_of/2
        , address_of/1, value_of/1, set_value/2
        ]).


