#!/bin/bash -ex

if [ -e /dev/sda5 ]
then
    exit 0
fi

cd /
echo -e "n\ne\n2\n\n\nn\nl\n5\n\n40000000\nn\nl\n6\n\n\nw" | sudo fdisk /dev/sda || true
sudo partprobe
sudo mkfs.ext4 /dev/sda5
sudo mkfs.ext4 /dev/sda6
sudo mkdir /mnt/usr
sudo mkdir /mnt/home
sudo mount /dev/sda5 /mnt/usr
sudo mount /dev/sda6 /mnt/home
sudo sh -c 'ls -l /dev/disk/by-uuid/ | grep "/sda5$" | sed "s|.*:[0-9][0-9] \([0-9a-z\-]*\) -> .*$|UUID=\1 /usr ext4 defaults 1 1|" >> /etc/fstab'
sudo sh -c 'ls -l /dev/disk/by-uuid/ | grep "/sda6$" | sed "s|.*:[0-9][0-9] \([0-9a-z\-]*\) -> .*$|UUID=\1 /home ext4 defaults 1 1|" >> /etc/fstab'
sudo cp -ax /usr/* /mnt/usr
sudo cp -ax /home/* /mnt/home
sudo umount /mnt/usr
sudo umount /mnt/home

