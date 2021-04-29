[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
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

  <ol>
    <li>
      <a href="#built-with">Built With</a>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#deployment">Deployment</a></li>
      </ul>
    </li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>

<!-- ABOUT THE PROJECT -->

## Built With

- [Arch linux based system](https://archlinux.org/)
- [Paru](https://github.com/morganamilo/paru)

### Content

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

Andrés Hurtado - [@OsiNubis](https://t.me/OsiNubis99) - OsiNubis99@PM.me

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->

[contributors-shield]: https://img.shields.io/github/contributors/OsiNubis99/dotFiles.svg?style=for-the-badge
[contributors-url]: https://github.com/OsiNubis99/dotFiles/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/OsiNubis99/dotFiles.svg?style=for-the-badge
[forks-url]: https://github.com/OsiNubis99/dotFiles/network/members
[stars-shield]: https://img.shields.io/github/stars/OsiNubis99/dotFiles.svg?style=for-the-badge
[stars-url]: https://github.com/OsiNubis99/dotFiles/stargazers
[issues-shield]: https://img.shields.io/github/issues/OsiNubis99/dotFiles.svg?style=for-the-badge
[issues-url]: https://github.com/OsiNubis99/dotFiles/issues
[license-shield]: https://img.shields.io/github/license/OsiNubis99/dotFiles.svg?style=for-the-badge
[license-url]: https://github.com/OsiNubis99/dotFiles/blob/master/LICENSE.txt
[product-screenshot]: https://telegram.org/img/t_logo.svg?1
