#!/usr/bin/env ruby

require './dataset-extractor/dataset_extractor'

dataset_size = 100
DatasetExtractor.new.run(dataset_size)
