from setuptools import setup, find_packages

setup(
    name="pysymkell",
    version="0.1.0",
    description="Python bindings for Symkell symbolic mathematics library",
    author="Symkell Team",
    packages=find_packages(),
    package_data={
        "pysymkell": ["*.so", "*.dll", "*.dylib"],
    },
    python_requires=">=3.6",
    install_requires=[],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Intended Audience :: Science/Research",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Haskell",
        "Topic :: Scientific/Engineering :: Mathematics",
    ],
) 