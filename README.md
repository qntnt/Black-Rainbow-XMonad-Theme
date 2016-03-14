## Black Rainbow XMonad Theme
> XMonad Config

![Floating Terminal](https://raw.githubusercontent.com/quentunahelper/Black-Rainbow-XMonad-Theme/bbdf328898a018d4b79c98383542d4e280355ee8/images/terminalFloat.png)
![Video 1](https://raw.githubusercontent.com/quentunahelper/Black-Rainbow-XMonad-Theme/bbdf328898a018d4b79c98383542d4e280355ee8/images/video.png)
![Video 2](https://raw.githubusercontent.com/quentunahelper/Black-Rainbow-XMonad-Theme/bbdf328898a018d4b79c98383542d4e280355ee8/images/video2.png)

### Dependencies
* XMonad `xmonad`
* XMonad Extras `xmonad-contrib`
* XMobar `xmobar`
* Compton `compton`
* urxvt `rxvt-unicode`
* feh `feh`
* MPlayer `mplayer`
* Curl `curl`
* youtube-dl `youtube-dl`

### Installation

First install dependencies

```bash
sudo apt-get install xmonad xmonad-contrib compton xmobar rxvt-unicode feh mplayer curl youtube-dl
```

Then clone this repo into your home directory as `.xmonad`

Move `.Xdefaults` to your home directory and add `dutub` to your path.
From there, this should work as long as you use XMonad as your window manager.

### Configuration

Most of the configuration is done through `xmonad.hs`.

For easy theming, convenient color variables are listed at the top of the file.
