require 'mechanize'
require 'sanitize'
require 'logging'
require 'fileutils'

class Log
  def self.logger
    @logger ||=
      begin
        logger = Logging.logger['logger']
        logger.add_appenders(Logging.appenders.file('info.log'))
        logger
      end
  end
end


class Extractor
  def agent
    @agent ||= Mechanize.new
  end

  def articles
    page = agent.get(feed)
    page.search('//description').map do |node|
      Sanitize.clean(node.text)
    end
  end


end

class LinkCollector
  def collect(max_links)
    link_selector = Proc.new do |url|
      url.include?("/#{category_name}/") && !url.include?("/all")
    end

    is_content_present = Proc.new do |page|
      content_present?(page)
    end

    collect_links_recursively(main_page,
      link_selector, is_content_present, max_links)
  end

  def content_present?(page)
    page.search('.article').size > 0
  end

  def main_page
    "http://www.theguardian.com/#{category_name}"
  end

  def category_name
    raise 'Implement in subclass'
  end

  private

  def collect_links_recursively(root_url, link_selector, is_content_present, max_links)
    links = []
    todo = [root_url]
    done = []

    while links.count < max_links && todo.size > 0
      current_url = todo.pop
      Log.logger.info('checking: ' + current_url)

      begin
        page = agent.get(current_url)
      rescue => exception
        done << current_url
        next
      end

      # collect child links
      link_urls = extract_link_urls_from_page(page, link_selector)


      # add only new links to the 'todo' list
      # new - those, that aren't already in the 'done' array
      todo.concat(link_urls - done)

      done.concat(link_urls)

      if is_content_present.call(page)
        links << current_url
        links.uniq!
        Log.logger.info('added url: ' + current_url)
        Log.logger.info('links collected: ' + links.count.to_s)
        Log.logger.info('todo queue size: ' + todo.count.to_s)
      end
    end

    links
  end

  def extract_link_urls_from_page(page, link_selector)
    page.links.map(&:href).compact.uniq.select do |link|
      link_selector.call(link)
    end
  end

  def agent
    @agent ||= Mechanize.new
  end
end

class SportLinkCollector < LinkCollector
  def category_name
    'sport'
  end
end


class TechLinkCollector < LinkCollector
  def category_name
    'technology'
  end
end

class CultureLinkCollector < LinkCollector
  def category_name
    'culture'
  end
end

class BusinessLinkCollector < LinkCollector
  def category_name
    'business'
  end
end

class EnvironmentLinkCollector < LinkCollector
  def category_name
    'environment'
  end
end


class ContentExtractor
  def extract_content(page)
    Log.logger.info('extracting content from: ' + page.uri.to_s)
    page.search('.article p').text
  end
end


class DatasetExtractor
  def extract_dataset(directory_name, size, link_collector, content_extractor)
    Log.logger.info('creating directory: ' + directory_name)
    create_directory(File.join('dataset', directory_name))

    links = link_collector.collect(size)
    counter = 1

    links.each do |href|
      content = content_extractor.extract_content(agent.get(href))
      file_name = counter.to_s

      Log.logger.info('writing file: ' + file_name)
      File.open(File.join('dataset', directory_name, file_name), 'w') do |file|
        file.puts(content)
      end

      counter += 1
    end
  end

  def create_directory(name)
    FileUtils.mkdir_p(name)
  end


  def agent
    @agent ||= Mechanize.new
  end

  def run(size)
    create_directory('dataset')
    extract_dataset('sport', size, SportLinkCollector.new, ContentExtractor.new)
    extract_dataset('tech', size, TechLinkCollector.new, ContentExtractor.new)
    extract_dataset('culture', size, CultureLinkCollector.new, ContentExtractor.new)
    extract_dataset('business', size, BusinessLinkCollector.new, ContentExtractor.new)
    extract_dataset('environment', size, EnvironmentLinkCollector.new, ContentExtractor.new)
  end
end
