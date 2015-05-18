Elm.Native.Transform2D.Extra = {};
Elm.Native.Transform2D.Extra.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Transform2D = elm.Native.Transform2D || {};
    elm.Native.Transform2D.Extra = elm.Native.Transform2D.Extra || {};
    if (elm.Native.Transform2D.Extra.values) return elm.Native.Transform2D.Extra.values;

   var Transform2D = Elm.Transform2D.make(elm);
   var Utils = Elm.Native.Utils.make(elm);

   function transform(t, p) {
   		var m11 = t[0], m12 = t[1], m21 = t[3], m22 = t[4], mdx = t[2], mdy = t[5];
		return Utils.Tuple2(m11*p._0+m12*p._1+mdx,m22*p._1+m21*p._0+mdy);
	}
    
   return elm.Native.Transform2D.Extra.values = {
        transform : F2(transform)
    };

};

