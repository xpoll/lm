package cn.blmdz.aide.file.aliyun;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.OpenOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.multipart.MultipartFile;

import com.aliyun.oss.OSSClient;
import com.aliyun.oss.model.ObjectMetadata;
import com.google.common.base.Throwables;

import cn.blmdz.aide.file.FileServer;
import cn.blmdz.aide.file.exception.FileException;
import cn.blmdz.aide.file.util.FUtil;

public class AliyunFileServer implements FileServer {
	private static final Logger log = LoggerFactory.getLogger(AliyunFileServer.class);
	private final String bucketName;
	private final OSSClient ossClient;

	public AliyunFileServer(String endpoint, String appKey, String appSecret, String bucketName) {
		this.bucketName = bucketName;
		this.ossClient = new OSSClient(endpoint, appKey, appSecret);
	}

	public String write(String path, MultipartFile file) throws FileException {
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(file.getContentType());
			metadata.setContentLength((long) file.getBytes().length);
			InputStream stream = file.getInputStream();
			Throwable var5 = null;

			try {
				this.ossClient.putObject(this.bucketName, FUtil.upPath(path), stream, metadata);
			} catch (Throwable var15) {
				var5 = var15;
				throw var15;
			} finally {
				if (stream != null) {
					if (var5 != null) {
						try {
							stream.close();
						} catch (Throwable var14) {
							var5.addSuppressed(var14);
						}
					} else {
						stream.close();
					}
				}

			}

			return path;
		} catch (Exception var17) {
			log.error("failed to upload file(path={}) to oss, cause:{}", path, Throwables.getStackTraceAsString(var17));
			throw new FileException(var17);
		}
	}

	public String write(String path, File file) throws FileException {
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(Files.probeContentType(file.toPath()));
			metadata.setContentLength(file.length());
			InputStream stream = Files.newInputStream(file.toPath(), new OpenOption[0]);
			Throwable var5 = null;

			try {
				this.ossClient.putObject(this.bucketName, FUtil.upPath(path), stream, metadata);
			} catch (Throwable var15) {
				var5 = var15;
				throw var15;
			} finally {
				if (stream != null) {
					if (var5 != null) {
						try {
							stream.close();
						} catch (Throwable var14) {
							var5.addSuppressed(var14);
						}
					} else {
						stream.close();
					}
				}

			}

			return path;
		} catch (Exception var17) {
			log.error("failed to upload file(path={}) to oss, cause:{}", path, Throwables.getStackTraceAsString(var17));
			throw new FileException(var17);
		}
	}

	public String write(String path, InputStream inputStream) throws FileException {
		String var4;
		try {
			ObjectMetadata metadata = new ObjectMetadata();
			metadata.setContentType(FUtil.contentType(path));
			metadata.setContentLength((long) FUtil.image(inputStream).getSize().intValue());
			this.ossClient.putObject(this.bucketName, FUtil.upPath(path), inputStream, metadata);
			var4 = path;
		} catch (Exception var13) {
			log.error("failed to upload file(path={}) to oss, cause:{}", path, Throwables.getStackTraceAsString(var13));
			throw new FileException(var13);
		} finally {
			if (inputStream != null) {
				try {
					inputStream.close();
				} catch (IOException var12) {
					log.error("Close file inputStream failed, path={}, error code={}", path,
							Throwables.getStackTraceAsString(var12));
				}
			}

		}

		return var4;
	}

	public boolean delete(String path) throws FileException {
		try {
			this.ossClient.deleteObject(this.bucketName, FUtil.upPath(path));
			return true;
		} catch (Exception var3) {
			log.error("failed to delete {} from oss, cause:{}", path, Throwables.getStackTraceAsString(var3));
			throw new FileException(var3);
		}
	}
}
