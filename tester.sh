#!/bin/zsh

echo foo > /tmp/tested
tee -a /tmp/tested
echo bar >> /tmp/tested
