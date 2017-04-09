package cn.blmdz.wolf.search.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.search.api.IndexTaskBuilder;
import cn.blmdz.home.search.api.model.IndexAction;
import cn.blmdz.home.search.api.model.IndexTask;
import cn.blmdz.wolf.search.dto.IndexedItem;
import cn.blmdz.wolf.search.item.SearchItemProperties;

@Component
public class IndexedItemIndexAction {
   private static final Logger log = LoggerFactory.getLogger(IndexedItemIndexAction.class);
   private final IndexTaskBuilder indexTaskBuilder;
   private final SearchItemProperties searchItemProperties;

   @Autowired
   public IndexedItemIndexAction(IndexTaskBuilder indexTaskBuilder, SearchItemProperties searchItemProperties) {
      this.indexTaskBuilder = indexTaskBuilder;
      this.searchItemProperties = searchItemProperties;
   }

   public IndexTask indexTask(IndexedItem indexedItem) {
      return this.indexTaskBuilder.indexName(this.searchItemProperties.getIndexName()).indexType(this.searchItemProperties.getIndexType()).indexAction(IndexAction.INDEX).build(indexedItem.getId(), indexedItem);
   }

   public IndexTask deleteTask(Long itemId) {
      return this.indexTaskBuilder.indexName(this.searchItemProperties.getIndexName()).indexType(this.searchItemProperties.getIndexType()).indexAction(IndexAction.DELETE).build(itemId, (Object)null);
   }
}
