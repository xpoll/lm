package cn.blmdz.home.search.api;

import java.io.StringWriter;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.mustachejava.Mustache;

import cn.blmdz.home.search.ESClient;
import cn.blmdz.home.search.IndexException;

@Component
public class Indexer {
   private static final Logger log = LoggerFactory.getLogger(Indexer.class);
   private final ESClient esClient;
   private final Mustacher mustacher;

   @Autowired
   public Indexer(ESClient esClient, Mustacher mustacher) {
      this.esClient = esClient;
      this.mustacher = mustacher;
   }

   public void bulk(String indexName, String indexType, String templateKey, List indexable) {
      Mustache m = this.mustacher.forPath(templateKey);

      try {
         StringWriter writer = new StringWriter();
         Throwable var7 = null;

         try {
            m.execute(writer, indexable).flush();
            String payload = writer.toString();
            this.esClient.bulk(indexName, indexType, payload);
         } catch (Throwable var17) {
            var7 = var17;
            throw var17;
         } finally {
            if(writer != null) {
               if(var7 != null) {
                  try {
                     writer.close();
                  } catch (Throwable var16) {
                     var7.addSuppressed(var16);
                  }
               } else {
                  writer.close();
               }
            }

         }

      } catch (Exception var19) {
         log.error("failed to bulk index {}", indexable);
         throw new IndexException("bulk index failed", var19);
      }
   }
}
