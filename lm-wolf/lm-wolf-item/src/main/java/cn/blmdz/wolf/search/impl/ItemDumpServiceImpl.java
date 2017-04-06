package cn.blmdz.wolf.search.impl;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Function;
import com.google.common.base.Stopwatch;
import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.search.api.IndexExecutor;
import cn.blmdz.home.search.api.IndexTaskBuilder;
import cn.blmdz.home.search.api.model.IndexTask;
import cn.blmdz.wolf.item.impl.dao.ItemAttributeDao;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.search.dto.IndexedItem;
import cn.blmdz.wolf.parana.search.item.ItemDumpService;
import cn.blmdz.wolf.parana.search.item.SearchItemProperties;

@Component
public class ItemDumpServiceImpl implements ItemDumpService {
   private static final Logger log = LoggerFactory.getLogger(ItemDumpServiceImpl.class);
   private static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
   private final ItemDao itemDao;
   private final ItemAttributeDao itemAttributeDao;
   private final IndexExecutor indexExecutor;
   private final IndexedItemFactory indexedItemFactory;
   private final IndexedItemIndexAction indexedItemIndexAction;
   private final SearchItemProperties searchItemProperties;

   @Autowired
   public ItemDumpServiceImpl(ItemDao itemDao, ItemAttributeDao itemAttributeDao, IndexExecutor indexExecutor, IndexedItemFactory indexedItemFactory, IndexTaskBuilder indexTaskBuilder, IndexedItemIndexAction indexedItemIndexAction, SearchItemProperties searchItemProperties) {
      this.itemDao = itemDao;
      this.itemAttributeDao = itemAttributeDao;
      this.indexExecutor = indexExecutor;
      this.indexedItemFactory = indexedItemFactory;
      this.indexedItemIndexAction = indexedItemIndexAction;
      this.searchItemProperties = searchItemProperties;
   }

   public Response fullDump() {
      try {
         log.info("full dump start");
         Integer fullDumpRange = this.searchItemProperties.getFullDumpRange();
         if(fullDumpRange.intValue() <= 0) {
            fullDumpRange = Integer.valueOf(1095);
         }

         String since = DATE_TIME_FORMAT.print(DateTime.now().withTimeAtStartOfDay().minusDays(fullDumpRange.intValue()));
         Stopwatch watch = Stopwatch.createStarted();
         int allIndexed = this.doIndex(since);
         watch.stop();
         log.info("item full dump end, cost: {} ms, dumped {} items", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)), Integer.valueOf(allIndexed));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to execute items full dump, cause: {}", Throwables.getStackTraceAsString(var5));
         return Response.fail("item.dump.fail");
      }
   }

   public Response deltaDump(Integer interval) {
      try {
         log.info("item delta dump start");
         Stopwatch watch = Stopwatch.createStarted();
         String since = DATE_TIME_FORMAT.print((new DateTime()).minusMinutes(interval.intValue()));
         int allIndexed = this.doIndex(since);
         watch.stop();
         log.info("item delta dump end, cost: {} ms, dumped {} items", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)), Integer.valueOf(allIndexed));
         return Response.ok(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("failed to execute items delta dump, cause: {}", Throwables.getStackTraceAsString(var5));
         return Response.fail("item.dump.fail");
      }
   }

   private int doIndex(String since) {
      Long lastId = Long.valueOf(this.itemDao.maxId().longValue() + 1L);
      int allIndexed = 0;

      while(true) {
         List<Item> items = this.itemDao.listSince(lastId, since, this.searchItemProperties.getBatchSize().intValue());
         if(Iterables.isEmpty(items)) {
            break;
         }

         List<Long> itemIds = Lists.transform(items, new Function<Item, Long>() {
            public Long apply(Item item) {
               return item.getId();
            }
         });
         List<ItemAttribute> itemAttributes = this.itemAttributeDao.findByItemIds(itemIds);
         Map<Long, ItemAttribute> byItemId = Maps.uniqueIndex(itemAttributes, new Function<ItemAttribute, Long>() {
            public Long apply(ItemAttribute itemId) {
               return itemId.getItemId();
            }
         });

         for(Item item : items) {
            try {
               if(item.getStatus().intValue() > 0) {
                  IndexedItem indexedItem = this.indexedItemFactory.create(item, (ItemAttribute)byItemId.get(item.getId()), new Object[0]);
                  IndexTask indexTask = this.indexedItemIndexAction.indexTask(indexedItem);
                  this.indexExecutor.submit(indexTask);
               } else {
                  IndexTask indexTask = this.indexedItemIndexAction.deleteTask(item.getId());
                  this.indexExecutor.submit(indexTask);
               }
            } catch (Exception var12) {
               log.error("failed to index item(id={}),cause:{}", item.getId(), Throwables.getStackTraceAsString(var12));
            }
         }

         allIndexed += items.size();
         log.info("has indexed {} items,and last handled id is {}", Integer.valueOf(allIndexed), lastId);
         lastId = ((Item)Iterables.getLast(items)).getId();
         if(items.size() < this.searchItemProperties.getBatchSize().intValue()) {
            break;
         }
      }

      return allIndexed;
   }
}
