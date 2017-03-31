package cn.blmdz.home.search.api.model;

import java.io.Serializable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.home.search.utils.JsonUtil;

public class IndexTask implements Serializable, Runnable {
   private static final Logger log = LoggerFactory.getLogger(IndexTask.class);
   private static final long serialVersionUID = 7763861093209386565L;
   private final ESClient esClient;
   private final ObjectMapper objectMapper;
   private final String indexName;
   private final String indexType;
   private final Object id;
   private final Object indexable;
   private final IndexAction action;

   public IndexTask(ESClient esClient, String indexName, String indexType, IndexAction action, Object id, Object indexable) {
      this.esClient = esClient;
      this.objectMapper = JsonUtil.JSON_NON_EMPTY_MAPPER;
      this.indexName = indexName;
      this.indexType = indexType;
      this.id = id;
      this.indexable = indexable;
      this.action = action;
   }

   public void run() {
      switch(this.action) {
      case INDEX:
         if(log.isDebugEnabled()) {
            log.debug("submit {} to index(name={},type={})", this.indexable);
         }

         try {
            this.esClient.index(this.indexName, this.indexType, this.id, this.objectMapper.writeValueAsString(this.indexable));
         } catch (Exception var2) {
            log.error("failed to index {}, cause:{}", this.indexable, Throwables.getStackTraceAsString(var2));
         }
         break;
      case DELETE:
         this.esClient.delete(this.indexName, this.indexType, this.id);
      }

   }

   public String toString() {
      return MoreObjects.toStringHelper(this).add("indexName", this.indexName).add("indexType", this.indexType).add("id", this.id).add("indexable", this.indexable).add("action", this.action).omitNullValues().toString();
   }
}
