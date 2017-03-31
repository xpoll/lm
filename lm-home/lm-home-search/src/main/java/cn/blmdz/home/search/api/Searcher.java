package cn.blmdz.home.search.api;

import java.io.StringWriter;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.BeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.mustachejava.Mustache;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.home.search.IndexException;
import cn.blmdz.home.search.api.model.Pagination;
import cn.blmdz.home.search.api.model.WithAggregations;
import cn.blmdz.home.search.model.Bucket;
import cn.blmdz.home.search.model.ESSearchResponse;
import cn.blmdz.home.search.model.Hit;
import cn.blmdz.home.search.model.TermsAggregation;

@Component
public class Searcher {
   private static final Logger log = LoggerFactory.getLogger(Searcher.class);
   private final ESClient esClient;
   private final Mustacher mustacher;

   @Autowired
   public Searcher(ESClient esClient, Mustacher mustacher) {
      this.esClient = esClient;
      this.mustacher = mustacher;
   }

   public <T> Pagination<T> search(String indexName, String indexType, String templateKey, Object criteria, Class<T> clazz) {
      Mustache m = this.mustacher.forPath(templateKey);

      try {
         StringWriter writer = new StringWriter();
         Throwable var8 = null;

         Pagination var14;
         try {
            m.execute(writer, criteria).flush();
            String payload = writer.toString();
            ESSearchResponse response = this.esClient.search(indexName, indexType, payload);
            Long total = response.getHits().getTotal();
            List<Hit> hits = response.getHits().getHits();
            List<T> result = this.toPojo(clazz, hits);
            var14 = new Pagination(total, result);
         } catch (Throwable var24) {
            var8 = var24;
            throw var24;
         } finally {
            if(writer != null) {
               if(var8 != null) {
                  try {
                     writer.close();
                  } catch (Throwable var23) {
                     var8.addSuppressed(var23);
                  }
               } else {
                  writer.close();
               }
            }

         }

         return var14;
      } catch (Exception var26) {
         log.error("failed to search {}", criteria);
         throw new IndexException("search by template failed", var26);
      }
   }

   public ESSearchResponse rawSearch(String indexName, String indexType, String templateKey, Object criteria) {
      Mustache m = this.mustacher.forPath(templateKey);

      try {
         StringWriter writer = new StringWriter();
         Throwable var7 = null;

         ESSearchResponse var9;
         try {
            m.execute(writer, criteria).flush();
            String payload = writer.toString();
            var9 = this.esClient.search(indexName, indexType, payload);
         } catch (Throwable var19) {
            var7 = var19;
            throw var19;
         } finally {
            if(writer != null) {
               if(var7 != null) {
                  try {
                     writer.close();
                  } catch (Throwable var18) {
                     var7.addSuppressed(var18);
                  }
               } else {
                  writer.close();
               }
            }

         }

         return var9;
      } catch (Exception var21) {
         log.error("failed to search {}", criteria);
         throw new IndexException("search by template failed", var21);
      }
   }

   public <T> WithAggregations<T> searchWithAggs(String indexName, String indexType, String templateKey, Object criteria, Class<T> clazz) {
      Mustache m = this.mustacher.forPath(templateKey);

      try {
         StringWriter writer = new StringWriter();
         Throwable var8 = null;

         WithAggregations var29;
         try {
            m.execute(writer, criteria).flush();
            String payload = writer.toString();
            ESSearchResponse response = this.esClient.search(indexName, indexType, payload);
            Long total = response.getHits().getTotal();
            List<Hit> hits = response.getHits().getHits();
            List<T> result = this.toPojo(clazz, hits);
            Map<String, TermsAggregation> aggregations = response.getAggregations();
            Map<String, List<Bucket>> aggs = Maps.newLinkedHashMap();
            if(aggregations != null) {
               for(String key : aggregations.keySet()) {
                  aggs.put(key, ((TermsAggregation)aggregations.get(key)).getBuckets());
               }
            }

            var29 = new WithAggregations(total, result, aggs);
         } catch (Throwable var26) {
            var8 = var26;
            throw var26;
         } finally {
            if(writer != null) {
               if(var8 != null) {
                  try {
                     writer.close();
                  } catch (Throwable var25) {
                     var8.addSuppressed(var25);
                  }
               } else {
                  writer.close();
               }
            }

         }

         return var29;
      } catch (Exception var28) {
         log.error("failed to search {}", criteria);
         throw new IndexException("search by template failed", var28);
      }
   }

   private <T> List<T> toPojo(Class<T> clazz, List<Hit> hits) throws Exception {
      List<T> result = Lists.newArrayListWithCapacity(hits.size());

      for(Hit hit : hits) {
         T entity = clazz.newInstance();
         BeanUtils.populate(entity, hit.get_source());
         if(hit.getHighlight() != null && !hit.getHighlight().isEmpty()) {
            BeanUtils.populate(entity, hit.getHighlight());
         }

         result.add(entity);
      }

      return result;
   }
}
