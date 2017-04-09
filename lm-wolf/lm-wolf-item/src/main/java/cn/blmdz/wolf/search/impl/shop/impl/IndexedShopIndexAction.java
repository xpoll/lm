package cn.blmdz.wolf.search.impl.shop.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.search.api.IndexTaskBuilder;
import cn.blmdz.home.search.api.model.IndexAction;
import cn.blmdz.home.search.api.model.IndexTask;
import cn.blmdz.wolf.search.dto.IndexedShop;
import cn.blmdz.wolf.search.shop.SearchShopProperties;

@Component
public class IndexedShopIndexAction
{
  private final IndexTaskBuilder indexTaskBuilder;
  private final SearchShopProperties searchShopProperties;

  @Autowired
  public IndexedShopIndexAction(IndexTaskBuilder indexTaskBuilder, SearchShopProperties searchShopProperties)
  {
    this.indexTaskBuilder = indexTaskBuilder;
    this.searchShopProperties = searchShopProperties;
  }

  public IndexTask indexTask(IndexedShop indexedShop) {
    return this.indexTaskBuilder.indexName(this.searchShopProperties.getIndexName()).indexType(this.searchShopProperties.getIndexType()).indexAction(IndexAction.INDEX).build(indexedShop.getId(), indexedShop);
  }

  public IndexTask deleteTask(Long shopId)
  {
    return this.indexTaskBuilder.indexName(this.searchShopProperties.getIndexName()).indexType(this.searchShopProperties.getIndexType()).indexAction(IndexAction.DELETE).build(shopId, null);
  }
}