## 1.1.0 - 2022-06-??
* 50% performance increase with zero change to the public API
  * This comes from using global variables for state instead of an expensive hash table
* Use GitHub Actions to test across Emacs 24.1~28.1

## 1.0.0 - 2016-08-13
* First Semantic Versioning Major Release :)
* Fast-forward to parinfer.js v1.8.0. Thanks [Kurvivor19]!
  * adds `previewCursorScope` and `tabStops` functionality
* Update the API to be simpler and closer to parinfer.js
* Updated test files and more thorough testing harness

## 0.2.0 - 2016-02-07
* Some code cleanup

## 0.1.0 - 2016-02-06 - First Release
* Initial release! [Parinfer] is now usable from Emacs :)

[Parinfer]:http://shaunlebron.github.io/parinfer/
[Kurvivor19]:https://github.com/Kurvivor19
