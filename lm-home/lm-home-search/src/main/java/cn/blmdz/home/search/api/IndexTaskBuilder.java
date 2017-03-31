package cn.blmdz.home.search.api;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.home.search.api.model.IndexInfo;

@Component
public class IndexTaskBuilder {
   private final ESClient esClient;

   @Autowired
   public IndexTaskBuilder(ESClient esClient) {
      this.esClient = esClient;
   }

   public IndexInfo indexName(String indexName) {
      return new IndexInfo(this, indexName);
   }

   public ESClient getEsClient() {
      return this.esClient;
   }
}
