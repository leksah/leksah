#!/bin/bash -ex

if [ ! -e /dev/sda5 ] || [ ! -e /mnt/usr ]
then
    exit 0
fi

sudo mount --bind / /mnt/usr
sudo rm -rf /mnt/usr/usr/*
sudo rm -rf /mnt/usr/home/*
sudo umount /mnt/usr
sudo rmdir /mnt/usr

