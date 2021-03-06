<p align="center">
<img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" style="margin: 0 auto;">
</p>
<h1 align="center">Luna programming language</h1>
<h3 align="center">
Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.
</h3>

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and 
refactor any application simply by connecting visual elements together. 
Collaborate with co-workers, interactively fine tune parameters, inspect the 
results and visually profile the performance in real-time.

Visit [The Luna Website](http://www.luna-lang.org) to learn more!

This repository contains the Luna compiler core and its command line version. 
For the full (visual) Luna Studio, please take a look at the 
[Luna Studio](https://github.com/luna/luna-studio) repository. For installation 
and management tools, there is 
[Luna Manager](https://github.com/luna/luna-manager).

## Getting Started
This will get you up and running for Luna development, with only a minimal 
amount of setup required.

Luna's build system is nice and simple, allowing you to bootstrap the compiler
as long as you have an installation of 
[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and the
Haskell parser generator `happy`. 

You can install the latter just by running `stack install happy`, which should
build the tool for your system and put it in your `stack` binary folder. 

### System Requirements
Luna runs on all reasonably new Linuxes, MacOS, and Windows. Luna was mostly 
tested on Ubuntu >= 14.04, Fedora >= 23, MacOS >= 10.11 (El Capitan) and Windows
10, although it should run fine on all Linux distros like Mint, Debian or Arch. 
Please report any issues here on GitHub or shoot an email to 
[contact@luna-lang.org](mailto:contact@luna-lang.org).

### Getting the Sources
Given you've probably been reading this document on GitHub, you might have an 
inkling where to look!. You can clone Luna using two methods:

- **Via HTTPS:** We recommend you only use HTTPS if checking out the sources as
  read-only. 

```
git clone https://github.com/luna/luna.git
```

- **Via SSH:** For those who plan on regularly making direct commits, cloning
  over SSH may provide a better user experience (but requires setting up your 
  SSH Keys with GitHub).

```
git clone git@github.com:luna/luna.git
```

### Building Luna
To build the command-line compiler interface along with all its sub-components, 
you will need to build the `shell` project. The instructions below assume that 
your `luna` repo is already cloned and we will refer to its location as 
`$LUNA_REPO_PATH`.

```
$ cd $LUNA_REPO_PATH/shell
$ stack install
```

Note that the executable for the compiler will be located in 
`$LUNA_REPO_PATH/dist/bin/public/luna` folder. You may wish to add it to your 
`$PATH`.

Additionally, if you intend to simply use the Luna compiler (as opposed to 
tinkering with it, which requires frequent rebuilds), you may consider adding 
`--ghc-options="-O2 -j4"` to the stack install command. This should make the 
Luna compiler run considerably faster, at the cost of longer build times for 
building it.

### Running Luna
As a prerequisite, you need to set a `LUNA_HOME` variable to point to the 
location of the Luna standard library. Assuming your repo is at 
`$LUNA_REPO_PATH`, you will need to set `LUNA_HOME` to `$LUNA_REPO_PATH/stdlib`.

Next, you need to create the project: create a directory structure like this:

```
your_project/
 ┖─ src/
    ┠─ Main.luna
    ┖─ any other *.luna files
```
A sample `Main.luna` file may look like this:

```python
import Std.Base

def main:
    print "Hello world"
    print (2 + 2)
```

To compile and run the project, simply type `luna` in the main project 
directory.

## Contributing to Luna
If you are interested in contributing to the development of Luna, please read
the 
[`CONTRIBUTING.md`](https://github.com/luna/luna/blob/master/CONTRIBUTING.md)
file. 

## License
This repository is licensed under the
[Apache 2.0](https://opensource.org/licenses/apache-2.0), as specified in the
[LICENSE](https://github.com/luna/luna/blob/master/LICENSE) file. 

Please be aware that, as the commercial backing for Luna, 
**New Byte Order Sp. z o. o.** reserves the right under the CLA to use 
contributions made to this repository as part of commercially available Luna 
products. 

If these terms are unacceptable to you, please do not contribute to the 
repository.

### The Contributor License Agreement
As part of your first contribution to this repository, you need to accept the 
Contributor License Agreement. You will automatically be asked to sign the CLA 
when you make your first pull request. 

Any work intentionally submitted for inclusion in Luna shall be licensed under
this CLA.

The CLA you sign applies to all repositories associated with the Luna project 
([Luna](https://github.com/luna/luna-rfcs), 
[Luna Studio](https://github.com/luna/luna-studio), etc), so you will only have 
to sign it once at the start of your contributions. 
