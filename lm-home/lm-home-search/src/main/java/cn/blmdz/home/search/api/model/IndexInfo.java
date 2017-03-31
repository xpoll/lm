package cn.blmdz.home.search.api.model;

import java.io.Serializable;

import com.google.common.base.Preconditions;

import cn.blmdz.home.search.api.IndexTaskBuilder;

public class IndexInfo implements Serializable {
   private final IndexTaskBuilder indexTaskBuilder;
   private final String indexName;
   private String indexType;
   private IndexAction indexAction;

   public IndexInfo(IndexTaskBuilder indexTaskBuilder, String indexName) {
      this.indexTaskBuilder = indexTaskBuilder;
      this.indexName = indexName;
   }

   public IndexInfo indexType(String indexType) {
      this.indexType = indexType;
      return this;
   }

   public IndexInfo indexAction(IndexAction indexAction) {
      this.indexAction = indexAction;
      return this;
   }

   public IndexTask build(Object id, Object indexable) {
      Preconditions.checkNotNull(this.indexTaskBuilder.getEsClient(), "esClient can not be null");
      Preconditions.checkNotNull(this.indexName, "indexName can not be null");
      Preconditions.checkNotNull(this.indexType, "indexType can not be null");
      Preconditions.checkNotNull(this.indexAction, "indexAction can not be null");
      Preconditions.checkNotNull(id, "id can not be null");
      return new IndexTask(this.indexTaskBuilder.getEsClient(), this.indexName, this.indexType, this.indexAction, id, indexable);
   }
}
