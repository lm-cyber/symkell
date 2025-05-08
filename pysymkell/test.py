from ctypes import *
import glob
import struct
import itertools

# For finding where the built .so file is, independent on whether it was built with stack or cabal
# def find_file_ending_with(ending_with_str, path='.'):
#     # `glob` `**` does not match dotfiles (such as `.stack_work`), so we have to do that explicitly.
#     glob_result_generator = itertools.chain(
#         glob.glob(path + "/**/*" + ending_with_str, recursive=True),
#         glob.glob(path + "/.*/**/*" + ending_with_str, recursive=True),
#     )
#     for path_str in glob_result_generator:
#         return path_str
#     else:
#         raise Exception("Could not find " + ending_with_str + " in " + path)
# so_file_path = find_file_ending_with('libsymkell-ffi.so', '../symkell/.stack-work/dist/x86_64-linux/ghc-9.6.6/build/')

lib = cdll.LoadLibrary("/home/void/symkell/symkell/.stack-work/dist/x86_64-linux/ghc-9.6.6/build/libHSsymkell-0.1.0.0-IW9vRnYO41zFcmJCwRX5gV-ghc9.6.6.so")

# lib.checkStringHS.argtypes = [c_char_p]
# lib.checkStringHS.restype = c_char_p

lib.example_init()

for x in range(4,8):
    print( lib.fibonacciHS(x))
    # print( lib.factorialHS(x))
print('Test')

# input_str = "Hello ADSADSA"
# result = lib.checkStringHS(input_str.encode('utf-8'))
# if result:
#     print(result.decode('utf-8'))

lib.example_exit()
