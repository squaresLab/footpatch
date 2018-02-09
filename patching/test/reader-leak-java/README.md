The spec I want is the first POST in this footprint:

```
PRE:
  this|callee = _t$208 formal:class java.io.InputStreamReader *( sub ) ;
  this|callee|->{java.io.InputStreamReader.in:_t$209 rearrange:(z)45} rearrange:(z)45:class java.io.InputStreamReader ( sub ) ; 
  _t$209|->{} rearrange:(z)45:class java.io.InputStream ( sub )

POST 1 of 2:
MEMne < void InputStream.close():45 >  {vpath: InputStreamReader.in}(_t$209) ;                  <- CLOSES, NO EXN.
return|callee = _t$210 initial:void  ;
this|callee|->{java.io.InputStreamReader.in:_t$209 formal(z)} formal(z):class java.io.InputStreamReader ( sub ) ; 
_t$209|->{} formal(z):class java.io.InputStream ( sub )

POST 2 of 2:                                                                                    <- CLOSES, WITH EXN
MEMne < void InputStream.close():45 >  {vpath: }(_t$210) ;  MEMne < void InputStream.close():45 >  {vpath: InputStreamReader.in}(_t$209) ;
return|callee = EXN _t$210 update:45:void  ;
this|callee|->{java.io.InputStreamReader.in:_t$209 formal(z)} formal(z):class java.io.InputStreamReader ( sub ) ; 
_t$209|->{} formal(z):class java.io.InputStream ( sub ) ; 
_t$210|->{java.lang.Throwable.cause:0 alloc, java.lang.Throwable.detailMessage:0 alloc, java.lang.Throwable.stackState:0 alloc, java.lang.Throwable.stackTrace:0 alloc, java.lang.Throwable.suppressedExceptions:0 alloc} alloc:class java.io.IOException 
```

This is in node30, search for

"START EXECUTING SPECS FOR void InputStreamReader.close() from state"
