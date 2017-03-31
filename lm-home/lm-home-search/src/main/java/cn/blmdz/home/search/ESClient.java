package cn.blmdz.home.search;

import cn.blmdz.home.search.IndexException;
import cn.blmdz.home.search.SearchException;
import cn.blmdz.home.search.model.ESSearchResponse;
import cn.blmdz.home.search.utils.JsonUtil;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Throwables;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ESClient {
   private static final Logger log = LoggerFactory.getLogger(ESClient.class);
   private final ObjectMapper objectMapper;
   private final String hostname;
   private final int port;

   public ESClient(String hostname, int port) {
      ObjectMapper objectMapper = JsonUtil.JSON_NON_EMPTY_MAPPER;
      this.objectMapper = objectMapper;
      this.hostname = hostname;
      this.port = port;
   }

   public boolean health() {
      try {
         String url = "http://" + this.hostname + ":" + this.port + "/_cluster/health";
         HttpRequest request = HttpRequest.get(url);
         if(this.isOk(request.code())) {
            log.info("cluster status :{}", request.body());
            return true;
         } else {
            return false;
         }
      } catch (Exception var3) {
         log.error("failed to elasticsearch check status ", var3);
         return false;
      }
   }

   public void createIndexIfNotExists(String indexName) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName;
      if(this.isOk(HttpRequest.head(url).code())) {
         log.info("index(name={}) has exists, skip", indexName);
      } else {
         HttpRequest request = HttpRequest.put(url);
         if(!this.isOk(request.code())) {
            log.error("failed to create index (indexName={}), http status:{}", indexName, Integer.valueOf(request.code()));
            throw new IndexException(request.body());
         } else {
            log.info("create index(name={}) success", indexName);
         }
      }
   }

   public void deleteIndex(String indexName) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName;
      HttpRequest request = HttpRequest.delete(url);
      if(!this.isOk(request.code())) {
         log.error("failed to delete index (indexName={}), http status:{}", indexName, Integer.valueOf(request.code()));
         throw new IndexException(request.body());
      } else {
         log.info("delete index(name={}) sucess", indexName);
      }
   }

   public void createMappingIfNotExists(String indexName, String indexType, String mapping) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName + "/" + indexType;
      if(this.isOk(HttpRequest.head(url).code())) {
         log.info("index mapping(indexName={}, indexType={}) has exists, skip", indexName, indexType);
      } else {
         HttpRequest request = HttpRequest.put(url + "/_mapping").send(mapping);
         if(!this.isOk(request.code())) {
            log.error("failed to create mapping for  (indexName={}, indexType={}, id={}), error:{}", new Object[]{indexName, indexType, Integer.valueOf(request.code())});
            throw new IndexException(request.body());
         } else {
            log.info("create mapping for Index(name={}, type={}) success, mapping:{}", new Object[]{indexName, indexType, mapping});
         }
      }
   }

   public void index(String indexName, String indexType, Object id, String document) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName + "/" + indexType + "/" + id;
      HttpRequest request = HttpRequest.post(url).send(document);
      if(!this.isOk(request.code())) {
         log.error("failed to index document (indexName={}, indexType={}, id={}), error:{}", new Object[]{indexName, indexType, id, Integer.valueOf(request.code())});
         throw new IndexException(request.body());
      }
   }

   private boolean isOk(int code) {
      return code >= 200 && code < 300;
   }

   public void delete(String indexName, String indexType, Object id) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName + "/" + indexType + "/" + id;
      HttpRequest request = HttpRequest.delete(url);
      if(!this.isOk(request.code())) {
         log.error("failed to delete document (indexName={}, indexType={}, id={}),error:{}", new Object[]{indexName, indexType, id, Integer.valueOf(request.code())});
         throw new IndexException(request.body());
      }
   }

   public void deleteByQuery(String indexName, String indexType, String criteria) {
   }

   public void bulk(String indexName, String indexType, String payload) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName + "/" + indexType + "/_bulk";
      HttpRequest request = HttpRequest.post(url).send(payload);
      if(!this.isOk(request.code())) {
         log.error("failed to bulk index (indexName={}, indexType={}, payload={}),error:{}", new Object[]{indexName, indexType, payload, Integer.valueOf(request.code())});
         throw new IndexException(request.body());
      }
   }

   public ESSearchResponse search(String indexName, String indexType, String criteria) {
      String url = "http://" + this.hostname + ":" + this.port + "/" + indexName + "/" + indexType + "/_search";
      HttpRequest request = HttpRequest.post(url).send(criteria);
      String body = request.body();
      if(!this.isOk(request.code())) {
         log.error("failed to search (indexName={}, indexType={}, criteria={}),error:{}", new Object[]{indexName, indexType, criteria, Integer.valueOf(request.code())});
         throw new SearchException(body);
      } else {
         try {
            return (ESSearchResponse)this.objectMapper.readValue(body, ESSearchResponse.class);
         } catch (Exception var8) {
            log.error("failed to deserialize es response:{}, cause:{}", body, Throwables.getStackTraceAsString(var8));
            throw new SearchException(var8);
         }
      }
   }
}
