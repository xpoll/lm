package cn.blmdz.wolf.search.impl.shop.impl;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.wolf.parana.search.shop.SearchShopProperties;
import cn.blmdz.wolf.search.impl.BaseESInitiator;

@Component
public class ESShopInitiator extends BaseESInitiator
{
  private static final Logger log = LoggerFactory.getLogger(ESShopInitiator.class);
  private final SearchShopProperties searchShopProperties;

  @Autowired
  public ESShopInitiator(ESClient esClient, SearchShopProperties searchShopProperties)
  {
    super(esClient);
    this.searchShopProperties = searchShopProperties;
  }

  @PostConstruct
  public void init() throws Exception {
    init(this.searchShopProperties.getIndexName(), this.searchShopProperties.getIndexType(), this.searchShopProperties.getMappingPath());
  }
}