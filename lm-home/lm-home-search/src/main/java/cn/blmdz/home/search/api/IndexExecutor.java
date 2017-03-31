package cn.blmdz.home.search.api;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.annotation.PreDestroy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public final class IndexExecutor {
   private static final Logger log = LoggerFactory.getLogger(IndexExecutor.class);
   private final ExecutorService executorService;

   @Autowired
   public IndexExecutor(@Value("${index.queue.size: 100000}") int queueSize) {
      this.executorService = new ThreadPoolExecutor(2, 4, 60L, TimeUnit.MINUTES, new ArrayBlockingQueue(queueSize), (new ThreadFactoryBuilder()).setNameFormat("search-indexer-%d").build(), new RejectedExecutionHandler() {
         public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
            IndexExecutor.log.error("task {} is rejected", r);
         }
      });
   }

   public void submit(Runnable r) {
      this.executorService.submit(r);
   }

   @PreDestroy
   public void shudown() {
      this.executorService.shutdown();
   }
}
