[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT DESCRIPTION -->
<p align="center">
  <a href="https://github.com/OsiNubis99/dotFiles">
    <img src="https://www.vectorlogo.zone/logos/linux/linux-icon.svg" alt="Logo" width="80" height="80">
  </a>
  <h3 align="center">DotFiles</h3>
  <p align="center">
    DotFiles are the customization files that are used to personalize your Linux or other Unix-based system.
    <br />
    <a href="https://github.com/OsiNubis99/dotFiles/issues">Report Bug</a>
    ·
    <a href="https://github.com/OsiNubis99/dotFiles/issues">Request Feature</a>
  </p>
</p>

<!-- TABLE OF CONTENTS -->

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Built With](#built-with)
  - [Content](#content)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Deployment](#deployment)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)
  - [Social medias](#social-medias)
  - [Email](#email)
- [Acknowledgements](#acknowledgements)

<!-- ABOUT THE PROJECT -->

## Built With

- [Arch linux based system](https://archlinux.org/)
- [Paru](https://github.com/morganamilo/paru)

### Content

- [Alacritty](https://github.com/OsiNubis99/dotFiles/tree/main/config/alacritty)
- [Dunst](https://github.com/OsiNubis99/dotFiles/tree/main/config/dunst)
- [Nitrogen](https://github.com/OsiNubis99/dotFiles/tree/main/config/nitrogen)
- [Picom](https://github.com/OsiNubis99/dotFiles/tree/main/config/picom)
- [Rofi](https://github.com/OsiNubis99/dotFiles/tree/main/config/rofi)
- [Scripts](https://github.com/OsiNubis99/dotFiles/tree/main/scripts)
- [Xmobar](https://github.com/OsiNubis99/dotFiles/tree/main/config/xmobar)
- [Xmonad](https://github.com/OsiNubis99/dotFiles/tree/main/config/xmonad)
- [ZSH](https://github.com/OsiNubis99/dotFiles/tree/main/config/zsh)

<!-- GETTING STARTED -->

## Getting Started

### Prerequisites

You need any linux distribution. I recommend an Arch linux based one.

### Deployment

1. Clone this repository

```sh
cd ~/
git clone https://github.com/OsiNubis99/dotFiles.git
```

2. Install Paru

```sh
bash ~/dotFiles/scripts/install-paru.sh
```

3. Install all recommended Apps

```sh
paru $(cat ~/dotFiles/apps) $(cat ~/dotFiles/aurApps)
```

4. Create all symbolic links

```sh
bash ~/dotFiles/startUp.sh
```

<!-- ROADMAP -->

## Roadmap

See the [open issues](https://github.com/OsiNubis99/dotFiles/issues) for a list of proposed features (and known issues).

<!-- CONTRIBUTING -->

## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<!-- LICENSE -->

## License

Distributed under the MIT License. See `LICENSE` for more information.

<!-- CONTACT -->

## Contact

### Social medias

[![Telegram](https://www.vectorlogo.zone/logos/telegram/telegram-icon.svg)](https://t.me/OsiNubis99)
[![Vue](https://www.vectorlogo.zone/logos/twitter/twitter-icon.svg)](https://twitter.com/OsiNubis99)

### Email

My email address OsiNubis99@PM.me

<!-- ACKNOWLEDGEMENTS -->

## Acknowledgements

- [Distro Tube](distrotube.com/)

<!-- MARKDOWN LINKS & IMAGES -->

[contributors-shield]: https://img.shields.io/github/contributors/OsiNubis99/dotFiles.svg?style=for-the-badge
[contributors-url]: https://github.com/OsiNubis99/dotFiles/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/OsiNubis99/dotFiles.svg?style=for-the-badge
[forks-url]: https://github.com/OsiNubis99/dotFiles/network/members
[stars-shield]: https://img.shields.io/github/stars/OsiNubis99/dotFiles.svg?style=for-the-badge
[stars-url]: https://github.com/OsiNubis99/dotFiles/stargazers
[issues-shield]: https://img.shields.io/github/issues/OsiNubis99/dotFiles.svg?style=for-the-badge
[issues-url]: https://github.com/OsiNubis99/dotFiles/issues
[license-shield]: https://img.shields.io/github/license/OsiNubis99/dotFiles.svg?style=for-the-badge
[license-url]: https://github.com/OsiNubis99/dotFiles/blob/master/LICENSE
