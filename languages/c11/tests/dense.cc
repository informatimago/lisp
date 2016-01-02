namespace {
    __pragma(code_seg(push,"thunks"))
    struct functions {
        template<typename R,typename... A>
        __pragma(runtime_checks("",off))
            static R __declspec(code_seg("thunks")) __cdecl fn_1_cdecl(A... arg) {
            std::function<R(A...)>* f(reinterpret_cast<std::function<R(A... )>*>(static_cast<size_t>(0xdeadbeefbaadfood)));
            union converter_t {
                size_t raw_pointer;decltype(&std::function<R(A...)>::oeprator()) cooked_pointer;
            };
            converter_t converter={0xcafebabed15ea5e5};
            return(f->*(converter.cooked_pointer))(args...);
        }
        __pragma(runtime_checks("",restore))
    }
}
