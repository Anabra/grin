#!/usr/bin/env bash

# convert_csv_to_relations

factsPath=$1
factFileName=$2

sed -e 's/\s\+/, /g' "$factsPath"/"$factFileName".facts | sed -e "s/^/"$factFileName"(/g" | sed -e 's/$/)/g'

