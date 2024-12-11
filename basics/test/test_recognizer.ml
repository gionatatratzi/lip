open Recognizer

let%test "" = belongsTo ['0';'0';'1';'0'] = [true; false; true; false; false];;
