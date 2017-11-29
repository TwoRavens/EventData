console.log("TEST");
import '../css/main.css';
import '../pkgs/jqtree/jqtree.style.css';

import m from 'mithril';

let root = document.body;
let Example = {
    view: function() {
        return m(root, "Hello")
    }
};

m(Example);
