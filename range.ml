
type 'a t = 
    { min : 'a;
      max : 'a
    }

let range min max = { min ; max }
