

struct Latin1String {
    data: ~[u8]
}



fn char_at(str : Latin1String, i: uint) -> u8 {
    return str.data[i];
}


fn test(str : Latin1String) -> u8 {

    return char_at(str, 0) + char_at(str, 1);
}
